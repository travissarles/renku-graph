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

package ch.datascience.webhookservice.hookcreation

import cats.MonadError
import cats.effect.IO
import ch.datascience.controllers.ErrorMessage
import ch.datascience.controllers.ErrorMessage._
import ch.datascience.generators.CommonGraphGenerators._
import ch.datascience.generators.Generators.Implicits._
import ch.datascience.graph.model.GraphModelGenerators.projectIds
import ch.datascience.graph.model.projects.Id
import ch.datascience.http.client.AccessToken
import ch.datascience.http.client.RestClientError.UnauthorizedException
import ch.datascience.http.server.EndpointTester._
import ch.datascience.webhookservice.hookcreation.HookCreator.CreationResult.{HookCreated, HookExisted}
import ch.datascience.webhookservice.security.IOAccessTokenExtractor
import io.circe.Json
import io.circe.literal._
import io.circe.syntax._
import org.http4s.Status._
import org.http4s._
import org.http4s.headers.`Content-Type`
import org.scalamock.scalatest.MockFactory
import org.scalatest.Matchers._
import org.scalatest.WordSpec

class HookCreationEndpointSpec extends WordSpec with MockFactory {

  "createHook" should {

    "return CREATED when a valid access token is present in the header " +
      "and webhook is successfully created for project with the given id in in GitLab" in new TestCase {

      val accessToken = accessTokens.generateOne
      (accessTokenFinder
        .findAccessToken(_: Request[IO]))
        .expects(*)
        .returning(context.pure(accessToken))

      (hookCreator
        .createHook(_: Id, _: AccessToken))
        .expects(projectId, accessToken)
        .returning(IO.pure(HookCreated))

      val request = Request[IO](Method.POST, uri"projects" / projectId.toString / "webhooks")

      val response = createHook(projectId, request).unsafeRunSync

      response.status                 shouldBe Created
      response.contentType            shouldBe Some(`Content-Type`(MediaType.application.json))
      response.as[Json].unsafeRunSync shouldBe json"""{"message": "Hook created"}"""
    }

    "return OK when hook was already created" in new TestCase {

      val accessToken = accessTokens.generateOne
      (accessTokenFinder
        .findAccessToken(_: Request[IO]))
        .expects(*)
        .returning(context.pure(accessToken))

      (hookCreator
        .createHook(_: Id, _: AccessToken))
        .expects(projectId, accessToken)
        .returning(IO.pure(HookExisted))

      val request = Request[IO](Method.POST, uri"projects" / projectId.toString / "webhooks")

      val response = createHook(projectId, request).unsafeRunSync()

      response.status                 shouldBe Ok
      response.contentType            shouldBe Some(`Content-Type`(MediaType.application.json))
      response.as[Json].unsafeRunSync shouldBe json"""{"message": "Hook already existed"}"""
    }

    "return UNAUTHORIZED when finding an access token in the headers fails with UnauthorizedException" in new TestCase {

      (accessTokenFinder
        .findAccessToken(_: Request[IO]))
        .expects(*)
        .returning(context.raiseError(UnauthorizedException))

      val request = Request[IO](Method.POST, uri"projects" / projectId.toString / "webhooks")

      val response = createHook(projectId, request).unsafeRunSync()

      response.status                 shouldBe Unauthorized
      response.contentType            shouldBe Some(`Content-Type`(MediaType.application.json))
      response.as[Json].unsafeRunSync shouldBe ErrorMessage(UnauthorizedException).asJson
    }

    "return INTERNAL_SERVER_ERROR when there was an error during hook creation" in new TestCase {

      val accessToken = accessTokens.generateOne
      (accessTokenFinder
        .findAccessToken(_: Request[IO]))
        .expects(*)
        .returning(context.pure(accessToken))

      val errorMessage = ErrorMessage("some error")
      (hookCreator
        .createHook(_: Id, _: AccessToken))
        .expects(projectId, accessToken)
        .returning(IO.raiseError(new Exception(errorMessage.toString())))

      val request = Request[IO](Method.POST, uri"projects" / projectId.toString / "webhooks")

      val response = createHook(projectId, request).unsafeRunSync()

      response.status                 shouldBe InternalServerError
      response.contentType            shouldBe Some(`Content-Type`(MediaType.application.json))
      response.as[Json].unsafeRunSync shouldBe errorMessage.asJson
    }

    "return UNAUTHORIZED when there was an UnauthorizedException thrown during hook creation" in new TestCase {

      val accessToken = accessTokens.generateOne
      (accessTokenFinder
        .findAccessToken(_: Request[IO]))
        .expects(*)
        .returning(context.pure(accessToken))

      val errorMessage = ErrorMessage("some error")
      (hookCreator
        .createHook(_: Id, _: AccessToken))
        .expects(projectId, accessToken)
        .returning(IO.raiseError(UnauthorizedException))

      val request = Request[IO](Method.POST, uri"projects" / projectId.toString / "webhooks")

      val response = createHook(projectId, request).unsafeRunSync()

      response.status                 shouldBe Unauthorized
      response.contentType            shouldBe Some(`Content-Type`(MediaType.application.json))
      response.as[Json].unsafeRunSync shouldBe ErrorMessage(UnauthorizedException).asJson
    }
  }

  private trait TestCase {
    val context = MonadError[IO, Throwable]

    val projectId = projectIds.generateOne

    val hookCreator       = mock[IOHookCreator]
    val accessTokenFinder = mock[IOAccessTokenExtractor]
    val createHook = new HookCreationEndpoint[IO](
      hookCreator,
      accessTokenFinder
    ).createHook _
  }
}
