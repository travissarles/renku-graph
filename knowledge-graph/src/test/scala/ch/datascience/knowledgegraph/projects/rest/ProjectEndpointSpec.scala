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

package ch.datascience.knowledgegraph.projects.rest

import cats.MonadError
import cats.effect.IO
import ch.datascience.controllers.InfoMessage._
import ch.datascience.controllers.{ErrorMessage, InfoMessage}
import ch.datascience.generators.CommonGraphGenerators.renkuResourcesUrls
import ch.datascience.generators.Generators.Implicits._
import ch.datascience.generators.Generators._
import ch.datascience.graph.model.GraphModelGenerators._
import ch.datascience.graph.model.projects._
import ch.datascience.graph.model.users.{Email, Name => UserName}
import ch.datascience.http.rest.Links
import ch.datascience.http.rest.Links.{Href, Rel}
import ch.datascience.http.server.EndpointTester._
import ch.datascience.interpreters.TestLogger
import ch.datascience.interpreters.TestLogger.Level.Error
import ch.datascience.knowledgegraph.projects.ProjectsGenerators._
import ch.datascience.knowledgegraph.projects.model._
import ch.datascience.tinytypes.json.TinyTypeDecoders._
import io.circe.syntax._
import io.circe.{Decoder, HCursor, Json}
import org.http4s.MediaType._
import org.http4s.Status._
import org.http4s._
import org.http4s.circe.jsonOf
import org.http4s.headers.`Content-Type`
import org.scalamock.scalatest.MockFactory
import org.scalatest.Matchers._
import org.scalatest.WordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class ProjectEndpointSpec extends WordSpec with MockFactory with ScalaCheckPropertyChecks {

  "getProject" should {

    "respond with OK and the found project details" in new TestCase {
      forAll { project: Project =>
        (projectFinder
          .findProject(_: ProjectPath))
          .expects(project.path)
          .returning(context.pure(Some(project)))

        val response = getProject(project.path).unsafeRunSync()

        response.status      shouldBe Ok
        response.contentType shouldBe Some(`Content-Type`(application.json))

        response.as[Project].unsafeRunSync shouldBe project
        response.as[Json].unsafeRunSync._links shouldBe Right(
          Links.of(
            Rel.Self        -> Href(renkuResourcesUrl / "projects" / project.path),
            Rel("datasets") -> Href(renkuResourcesUrl / "projects" / project.path / "datasets")
          )
        )

        logger.expectNoLogs()
      }
    }

    "respond with NOT_FOUND if there is no project with the given path" in new TestCase {

      val path = projectPaths.generateOne

      (projectFinder
        .findProject(_: ProjectPath))
        .expects(path)
        .returning(context.pure(None))

      val response = getProject(path).unsafeRunSync()

      response.status      shouldBe NotFound
      response.contentType shouldBe Some(`Content-Type`(application.json))

      response.as[Json].unsafeRunSync shouldBe InfoMessage(s"No '$path' project found").asJson

      logger.expectNoLogs()
    }

    "respond with INTERNAL_SERVER_ERROR if finding project details fails" in new TestCase {

      val path      = projectPaths.generateOne
      val exception = exceptions.generateOne
      (projectFinder
        .findProject(_: ProjectPath))
        .expects(path)
        .returning(context.raiseError(exception))

      val response = getProject(path).unsafeRunSync()

      response.status      shouldBe InternalServerError
      response.contentType shouldBe Some(`Content-Type`(application.json))

      response.as[Json].unsafeRunSync shouldBe ErrorMessage(s"Finding '$path' project failed").asJson

      logger.loggedOnly(Error(s"Finding '$path' project failed", exception))
    }
  }

  private trait TestCase {
    val context = MonadError[IO, Throwable]

    val projectFinder     = mock[ProjectFinder[IO]]
    val renkuResourcesUrl = renkuResourcesUrls.generateOne
    val logger            = TestLogger[IO]()
    val getProject        = new ProjectEndpoint[IO](projectFinder, renkuResourcesUrl, logger).getProject _
  }

  private implicit val projectEntityDecoder: EntityDecoder[IO, Project] = jsonOf[IO, Project]

  private implicit lazy val projectDecoder: Decoder[Project] = (cursor: HCursor) =>
    for {
      path    <- cursor.downField("path").as[ProjectPath]
      name    <- cursor.downField("name").as[Name]
      created <- cursor.downField("created").as[ProjectCreation]
    } yield Project(path, name, created)

  private implicit lazy val projectCreationDecoder: Decoder[ProjectCreation] = (cursor: HCursor) =>
    for {
      date    <- cursor.downField("dateCreated").as[DateCreated]
      creator <- cursor.downField("creator").as[ProjectCreator]
    } yield ProjectCreation(date, creator)

  private implicit lazy val projectCreatorDecoder: Decoder[ProjectCreator] = (cursor: HCursor) =>
    for {
      name  <- cursor.downField("name").as[UserName]
      email <- cursor.downField("email").as[Email]
    } yield ProjectCreator(email, name)
}