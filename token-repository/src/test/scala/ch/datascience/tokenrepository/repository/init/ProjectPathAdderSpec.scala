/*
 * Copyright 2020 Swiss Data Science Center (SDSC)
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

package ch.datascience.tokenrepository.repository.init

import cats.effect.IO
import cats.implicits._
import ch.datascience.generators.CommonGraphGenerators.accessTokens
import ch.datascience.generators.Generators.Implicits._
import ch.datascience.graph.model.EventsGenerators._
import ch.datascience.graph.model.GraphModelGenerators._
import ch.datascience.graph.model.projects.{Id, Path}
import ch.datascience.http.client.AccessToken
import ch.datascience.interpreters.TestLogger
import ch.datascience.interpreters.TestLogger.Level.Info
import ch.datascience.tokenrepository.repository.AccessTokenCrypto.EncryptedAccessToken
import ch.datascience.tokenrepository.repository.RepositoryGenerators.encryptedAccessTokens
import ch.datascience.tokenrepository.repository.association.ProjectPathFinder
import ch.datascience.tokenrepository.repository.deletion.TokenRemover
import ch.datascience.tokenrepository.repository.{IOAccessTokenCrypto, InMemoryProjectsTokensDbSpec}
import doobie.implicits._
import org.scalamock.scalatest.MockFactory
import org.scalatest.Matchers._
import org.scalatest.WordSpec
import org.scalatest.concurrent.{Eventually, IntegrationPatience}

class ProjectPathAdderSpec
    extends WordSpec
    with InMemoryProjectsTokensDbSpec
    with MockFactory
    with Eventually
    with IntegrationPatience {

  "run" should {

    "do nothing if the 'project_path' column already exists" in new TestCase {
      if (!tableExists()) createTable()
      addProjectPath()
      checkColumnExists shouldBe true

      projectPathAdder.run.unsafeRunSync() shouldBe ((): Unit)

      checkColumnExists shouldBe true

      logger.loggedOnly(Info("'project_path' column exists"))
    }

    "add the 'project_path' column if does not exist and add paths fetched from GitLab" in new TestCase {
      if (tableExists()) {
        dropTable()
        createTable()
      }
      checkColumnExists shouldBe false

      val project1Id             = projectIds.generateOne
      val project1Path           = projectPaths.generateOne
      val project1TokenEncrypted = encryptedAccessTokens.generateOne
      val project1Token          = accessTokens.generateOne
      insert(project1Id, project1TokenEncrypted)
      assumePathExistsInGitLab(project1Id, Some(project1Path), project1TokenEncrypted, project1Token)

      val project2Id             = projectIds.generateOne
      val project2Path           = projectPaths.generateOne
      val project2TokenEncrypted = encryptedAccessTokens.generateOne
      val project2Token          = accessTokens.generateOne
      insert(project2Id, project2TokenEncrypted)
      assumePathExistsInGitLab(project2Id, Some(project2Path), project2TokenEncrypted, project2Token)

      projectPathAdder.run.unsafeRunSync() shouldBe ((): Unit)

      eventually {
        findToken(project1Path) shouldBe Some(project1TokenEncrypted.value)
        findToken(project2Path) shouldBe Some(project2TokenEncrypted.value)
      }

      eventually {
        logger.loggedOnly(Info("'project_path' column added"))
      }
      eventually {
        verifyTrue(sql"DROP INDEX idx_project_path;")
      }
    }

    "add the 'project_path' column if does not exist and remove entries for non-existing projects in GitLab" in new TestCase {
      if (tableExists()) {
        dropTable()
        createTable()
      }
      checkColumnExists shouldBe false

      val project1Id             = projectIds.generateOne
      val project1TokenEncrypted = encryptedAccessTokens.generateOne
      val project1Token          = accessTokens.generateOne
      insert(project1Id, project1TokenEncrypted)
      assumePathExistsInGitLab(project1Id, None, project1TokenEncrypted, project1Token)

      val project2Id             = projectIds.generateOne
      val project2Path           = projectPaths.generateOne
      val project2TokenEncrypted = encryptedAccessTokens.generateOne
      val project2Token          = accessTokens.generateOne
      insert(project2Id, project2TokenEncrypted)
      assumePathExistsInGitLab(project2Id, Some(project2Path), project2TokenEncrypted, project2Token)

      projectPathAdder.run.unsafeRunSync() shouldBe ((): Unit)

      eventually {
        findToken(project2Path) shouldBe Some(project2TokenEncrypted.value)
      }

      eventually {
        verifyTrue(sql"DROP INDEX idx_project_path;")
      }

      logger.loggedOnly(Info("'project_path' column added"))
    }
  }

  private trait TestCase {
    val logger            = TestLogger[IO]()
    val accessTokenCrypto = mock[IOAccessTokenCrypto]
    val pathFinder        = mock[ProjectPathFinder[IO]]
    val tokenRemover      = new TokenRemover[IO](transactor)
    val projectPathAdder  = new IOProjectPathAdder(transactor, accessTokenCrypto, pathFinder, tokenRemover, logger)

    def assumePathExistsInGitLab(projectId:        Id,
                                 maybeProjectPath: Option[Path],
                                 encryptedToken:   EncryptedAccessToken,
                                 token:            AccessToken) = {
      (accessTokenCrypto.decrypt _)
        .expects(encryptedToken)
        .returning(token.pure[IO])
        .atLeastOnce()
      (pathFinder
        .findProjectPath(_: Id, _: Option[AccessToken]))
        .expects(projectId, Some(token))
        .returning(maybeProjectPath.pure[IO])
        .atLeastOnce()
    }
  }

  private def addProjectPath(): Unit = execute {
    sql"""
         |ALTER TABLE projects_tokens
         |ADD COLUMN project_path VARCHAR;
       """.stripMargin.update.run.map(_ => ())
  }

  private def checkColumnExists: Boolean =
    sql"select project_path from projects_tokens limit 1"
      .query[String]
      .option
      .transact(transactor.get)
      .map(_ => true)
      .recover { case _ => false }
      .unsafeRunSync()

  def insert(projectId: Id, encryptedToken: EncryptedAccessToken): Unit = execute {
    sql"""insert into
            projects_tokens (project_id, token)
            values (${projectId.value}, ${encryptedToken.value})
         """.update.run
      .map(assureInserted)
  }

  private lazy val assureInserted: Int => Unit = {
    case 1 => ()
    case _ => fail("insertion problem")
  }

  protected override def createTable(): Unit = execute {
    sql"""
         |CREATE TABLE projects_tokens(
         | project_id int4 PRIMARY KEY,
         | token VARCHAR NOT NULL
         |);
       """.stripMargin.update.run.map(_ => ())
  }
}
