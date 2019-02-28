/*
 * Copyright 2019 Swiss Data Science Center (SDSC)
 * A partnership between École Polytechnique Fédérale de Lausanne (EPFL) and
 * Eidgenössische Technische Hochschule Zürich (ETHZ).
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package ch.datascience.triplesgenerator.eventprocessing

import java.io._
import java.security.SecureRandom

import cats.MonadError
import cats.effect.{ContextShift, IO}
import cats.implicits._
import ch.datascience.config.ServiceUrl
import ch.datascience.graph.model.events.{CommitId, ProjectPath}
import ch.datascience.http.client.AccessToken
import ch.datascience.http.client.AccessToken.{OAuthAccessToken, PersonalAccessToken}
import ch.datascience.triplesgenerator.eventprocessing.Commands.GitLabRepoUrlFinder
import ch.datascience.triplesgenerator.eventprocessing.Commit._
import org.apache.jena.rdf.model.ModelFactory

import scala.language.higherKinds
import scala.util.control.NonFatal

private abstract class TriplesFinder[Interpretation[_]] {
  def generateTriples(commit: Commit, maybeAccessToken: Option[AccessToken]): Interpretation[RDFTriples]
}

private class IOTriplesFinder(
    gitRepoUrlFinder: GitLabRepoUrlFinder[IO],
    file:             Commands.File = new Commands.File,
    git:              Commands.Git = new Commands.Git,
    renku:            Commands.Renku = new Commands.Renku,
    toRdfTriples: InputStream => IO[RDFTriples] = inputStream =>
      IO(RDFTriples(ModelFactory.createDefaultModel.read(inputStream, ""))),
    randomLong:          () => Long = new SecureRandom().nextLong _
)(implicit contextShift: ContextShift[IO])
    extends TriplesFinder[IO] {

  import ammonite.ops.{Path, root}
  import file._
  import gitRepoUrlFinder._

  private val workDirectory: Path = root / "tmp"
  private val repositoryDirectoryFinder = ".*/(.*)$".r

  def generateTriples(commit: Commit, maybeAccessToken: Option[AccessToken]): IO[RDFTriples] =
    createRepositoryDirectory(commit.project.path)
      .bracket { repositoryDirectory =>
        for {
          gitRepositoryUrl <- findRepositoryUrl(commit.project.path, maybeAccessToken)
          _                <- git cloneRepo (gitRepositoryUrl, repositoryDirectory, workDirectory)
          _                <- git checkout (commit.id, repositoryDirectory)
          triplesStream    <- findTriplesStream(commit, repositoryDirectory)
          rdfTriples       <- toRdfTriples(triplesStream)
        } yield rdfTriples
      }(repositoryDirectory => delete(repositoryDirectory))
      .recoverWith(meaningfulError)

  private def createRepositoryDirectory(projectPath: ProjectPath): IO[Path] =
    contextShift.shift *> mkdir(tempDirectoryName(repositoryNameFrom(projectPath)))

  private def tempDirectoryName(repositoryName: String) =
    workDirectory / s"$repositoryName-${randomLong()}"

  private def repositoryNameFrom(projectPath: ProjectPath): String = projectPath.value match {
    case repositoryDirectoryFinder(folderName) => folderName
  }

  private def findTriplesStream(commit: Commit, repositoryDirectory: Path): IO[InputStream] = {
    import renku._

    commit match {
      case withParent:    CommitWithParent    => renku.log(withParent, repositoryDirectory)
      case withoutParent: CommitWithoutParent => renku.log(withoutParent, repositoryDirectory)
    }
  }

  private lazy val meaningfulError: PartialFunction[Throwable, IO[RDFTriples]] = {
    case NonFatal(exception) =>
      IO.raiseError(new RuntimeException("Triples generation failed", exception))
  }
}

private object Commands {

  class GitLabRepoUrlFinder[Interpretation[_]](
      gitLabUrlProvider: GitLabUrlProvider[Interpretation]
  )(implicit ME:         MonadError[Interpretation, Throwable]) {

    import cats.implicits._

    def findRepositoryUrl(projectPath: ProjectPath, maybeAccessToken: Option[AccessToken]): Interpretation[ServiceUrl] =
      for {
        gitLabUrl <- gitLabUrlProvider.get
        urlTokenPart = findUrlTokenPart(maybeAccessToken)
        finalUrl <- merge(gitLabUrl, urlTokenPart, projectPath)
      } yield finalUrl

    private lazy val findUrlTokenPart: Option[AccessToken] => String = {
      case None                             => ""
      case Some(PersonalAccessToken(token)) => s"gitlab-ci-token:$token@"
      case Some(OAuthAccessToken(token))    => s"oauth2:$token@"
    }

    private def merge(serviceUrl:   ServiceUrl,
                      urlTokenPart: String,
                      projectPath:  ProjectPath): Interpretation[ServiceUrl] = ME.fromEither {
      ServiceUrl.from {
        val url              = serviceUrl.value
        val serviceWithToken = url.toString.replace(s"${url.getProtocol}//:", s"${url.getProtocol}//:$urlTokenPart")
        s"$serviceWithToken/$projectPath.git"
      }
    }
  }

  import ammonite.ops
  import ammonite.ops._

  class File {

    def mkdir(newDir: Path): IO[Path] = IO {
      ops.mkdir ! newDir
      newDir
    }

    def delete(repositoryDirectory: Path): IO[Unit] = IO {
      ops.rm ! repositoryDirectory
    }
  }

  class Git {

    def cloneRepo(
        repositoryUrl:        ServiceUrl,
        destinationDirectory: Path,
        workDirectory:        Path
    ): IO[CommandResult] = IO {
      %%('git, 'clone, repositoryUrl.toString, destinationDirectory.toString)(workDirectory)
    }

    def checkout(
        commitId:            CommitId,
        repositoryDirectory: Path
    ): IO[CommandResult] = IO {
      %%('git, 'checkout, commitId.value)(repositoryDirectory)
    }
  }

  class Renku {

    def log[T <: Commit](
        commit:                 T,
        destinationDirectory:   Path
    )(implicit generateTriples: (T, Path) => CommandResult): IO[InputStream] = IO {
      generateTriples(commit, destinationDirectory).out.toInputStream
    }

    implicit val commitWithoutParentTriplesFinder: (CommitWithoutParent, Path) => CommandResult = {
      case (_, destinationDirectory) =>
        %%('renku, 'log, "--format", "rdf")(destinationDirectory)
    }

    implicit val commitWithParentTriplesFinder: (CommitWithParent, Path) => CommandResult = {
      case (commit, destinationDirectory) =>
        val changedFiles = %%(
          'git,
          'diff,
          "--name-only",
          s"${commit.parentId}..${commit.id}"
        )(destinationDirectory).out.lines

        %%(
          'renku,
          'log,
          "--format",
          "rdf",
          "--revision",
          s"${commit.parentId}..${commit.id}",
          changedFiles
        )(destinationDirectory)
    }

    private implicit class StreamValueOps(streamValue: StreamValue) {
      lazy val toInputStream: InputStream =
        new ByteArrayInputStream(streamValue.chunks.flatMap(_.array).toArray)
    }
  }
}