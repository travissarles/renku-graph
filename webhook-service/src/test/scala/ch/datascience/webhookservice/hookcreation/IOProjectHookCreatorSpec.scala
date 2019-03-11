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

package ch.datascience.webhookservice.hookcreation

import cats.effect.{ContextShift, IO}
import ch.datascience.generators.CommonGraphGenerators._
import ch.datascience.generators.Generators.Implicits._
import ch.datascience.generators.Generators._
import ch.datascience.stubbing.ExternalServiceStubbing
import ch.datascience.webhookservice.config.{GitLabConfigProvider, IOGitLabConfigProvider}
import ch.datascience.http.client.RestClientError.UnauthorizedException
import ch.datascience.webhookservice.hookcreation.HookCreationGenerators._
import ch.datascience.webhookservice.hookcreation.ProjectHookCreator.ProjectHook
import com.github.tomakehurst.wiremock.client.WireMock._
import eu.timepit.refined.api.{RefType, Refined}
import eu.timepit.refined.string.Url
import io.circe.Json
import org.http4s.Status
import org.scalamock.scalatest.MockFactory
import org.scalatest.Matchers._
import org.scalatest.WordSpec

import scala.concurrent.ExecutionContext.Implicits.global

class IOProjectHookCreatorSpec extends WordSpec with MockFactory with ExternalServiceStubbing {

  "create" should {

    "send relevant Json payload and 'PRIVATE-TOKEN' header (when Personal Access Token is given) " +
      "and return Unit if the remote responds with CREATED" in new TestCase {

      expectGitLabHostProvider(returning = IO.pure(gitLabUrl))
      val personalAccessToken = personalAccessTokens.generateOne

      stubFor {
        post(s"/api/v4/projects/$projectId/hooks")
          .withHeader("PRIVATE-TOKEN", equalTo(personalAccessToken.value))
          .withRequestBody(equalToJson(toJson(projectHook)))
          .willReturn(created())
      }

      hookCreator.create(projectHook, personalAccessToken).unsafeRunSync() shouldBe ((): Unit)
    }

    "send relevant Json payload and 'Authorization' header (when OAuth Access Token is given) " +
      "and return Unit if the remote responds with CREATED" in new TestCase {

      expectGitLabHostProvider(returning = IO.pure(gitLabUrl))
      val oauthAccessToken = oauthAccessTokens.generateOne

      stubFor {
        post(s"/api/v4/projects/$projectId/hooks")
          .withHeader("Authorization", equalTo(s"Bearer ${oauthAccessToken.value}"))
          .withRequestBody(equalToJson(toJson(projectHook)))
          .willReturn(created())
      }

      hookCreator.create(projectHook, oauthAccessToken).unsafeRunSync() shouldBe ((): Unit)
    }

    "return an error if gitLabUrl cannot be read" in new TestCase {
      val exception = exceptions.generateOne
      expectGitLabHostProvider(returning = IO.raiseError(exception))
      val accessToken = accessTokens.generateOne

      intercept[Exception] {
        hookCreator.create(projectHook, accessToken).unsafeRunSync()
      } shouldBe exception
    }

    "return an UnauthorizedException if remote client responds with UNAUTHORIZED" in new TestCase {
      expectGitLabHostProvider(returning = IO.pure(gitLabUrl))
      val accessToken = accessTokens.generateOne

      stubFor {
        post(s"/api/v4/projects/$projectId/hooks")
          .withRequestBody(equalToJson(toJson(projectHook)))
          .willReturn(unauthorized())
      }

      intercept[Exception] {
        hookCreator.create(projectHook, accessToken).unsafeRunSync()
      } shouldBe UnauthorizedException
    }

    "return an Exception if remote client responds with status neither CREATED nor UNAUTHORIZED" in new TestCase {
      expectGitLabHostProvider(returning = IO.pure(gitLabUrl))
      val accessToken = accessTokens.generateOne

      stubFor {
        post(s"/api/v4/projects/$projectId/hooks")
          .withRequestBody(equalToJson(toJson(projectHook)))
          .willReturn(badRequest().withBody("some message"))
      }

      intercept[Exception] {
        hookCreator.create(projectHook, accessToken).unsafeRunSync()
      }.getMessage shouldBe s"POST $gitLabUrl/api/v4/projects/$projectId/hooks returned ${Status.BadRequest}; body: some message"
    }
  }

  private implicit val cs: ContextShift[IO] = IO.contextShift(global)

  private trait TestCase {
    val projectHook = projectHooks.generateOne
    val projectId   = projectHook.projectId
    val gitLabUrl   = url(externalServiceBaseUrl)

    def toJson(projectHook: ProjectHook) =
      Json
        .obj(
          "id"          -> Json.fromInt(projectHook.projectId.value),
          "url"         -> Json.fromString(projectHook.projectHookUrl.value),
          "push_events" -> Json.fromBoolean(true),
          "token"       -> Json.fromString(projectHook.serializedHookToken.value)
        )
        .toString()

    val configProvider = mock[IOGitLabConfigProvider]
    val hookCreator    = new IOProjectHookCreator(configProvider)

    def expectGitLabHostProvider(returning: IO[GitLabConfigProvider.HostUrl]) =
      (configProvider.get _)
        .expects()
        .returning(returning)

    private def url(value: String) =
      RefType
        .applyRef[String Refined Url](value)
        .getOrElse(throw new IllegalArgumentException("Invalid url value"))
  }
}