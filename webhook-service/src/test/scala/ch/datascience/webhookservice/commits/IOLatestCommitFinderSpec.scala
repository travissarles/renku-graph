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

package ch.datascience.webhookservice.commits

import cats.effect.{ContextShift, IO, Timer}
import ch.datascience.control.Throttler
import ch.datascience.generators.CommonGraphGenerators.{accessTokens, oauthAccessTokens, personalAccessTokens}
import ch.datascience.generators.Generators.Implicits._
import ch.datascience.graph.config.GitLabUrl
import ch.datascience.graph.model.GraphModelGenerators._
import ch.datascience.http.client.RestClientError.UnauthorizedException
import ch.datascience.interpreters.TestLogger
import ch.datascience.stubbing.ExternalServiceStubbing
import ch.datascience.webhookservice.generators.WebhookServiceGenerators._
import com.github.tomakehurst.wiremock.client.WireMock._
import io.circe.Json
import io.circe.literal._
import org.http4s.Status
import org.scalamock.scalatest.MockFactory
import org.scalatest.Matchers._
import org.scalatest.WordSpec

import scala.concurrent.ExecutionContext.Implicits.global

class IOLatestCommitFinderSpec extends WordSpec with MockFactory with ExternalServiceStubbing {

  "findLatestCommit" should {

    "return latest Commit info if remote responds with OK and valid body - personal access token case" in new TestCase {
      val personalAccessToken = personalAccessTokens.generateOne

      stubFor {
        get(s"/api/v4/projects/$projectId/repository/commits?per_page=1")
          .withHeader("PRIVATE-TOKEN", equalTo(personalAccessToken.value))
          .willReturn(okJson(commitsJson(from = commitInfo)))
      }

      latestCommitFinder.findLatestCommit(projectId, Some(personalAccessToken)).value.unsafeRunSync() shouldBe Some(
        commitInfo
      )
    }

    "return latest Commit info if remote responds with OK and valid body - oauth token case" in new TestCase {
      val oauthAccessToken = oauthAccessTokens.generateOne

      stubFor {
        get(s"/api/v4/projects/$projectId/repository/commits?per_page=1")
          .withHeader("Authorization", equalTo(s"Bearer ${oauthAccessToken.value}"))
          .willReturn(okJson(commitsJson(from = commitInfo)))
      }

      latestCommitFinder.findLatestCommit(projectId, Some(oauthAccessToken)).value.unsafeRunSync() shouldBe Some(
        commitInfo
      )
    }

    "return latest Commit info if remote responds with OK and valid body - no token case" in new TestCase {

      stubFor {
        get(s"/api/v4/projects/$projectId/repository/commits?per_page=1")
          .willReturn(okJson(commitsJson(from = commitInfo)))
      }

      latestCommitFinder.findLatestCommit(projectId, maybeAccessToken = None).value.unsafeRunSync() shouldBe Some(
        commitInfo
      )
    }

    "return None if remote responds with OK and no commits" in new TestCase {

      stubFor {
        get(s"/api/v4/projects/$projectId/repository/commits?per_page=1")
          .willReturn(okJson("[]"))
      }

      latestCommitFinder.findLatestCommit(projectId, maybeAccessToken = None).value.unsafeRunSync() shouldBe None
    }

    "return None if remote responds with NOT_FOUND" in new TestCase {

      stubFor {
        get(s"/api/v4/projects/$projectId/repository/commits?per_page=1")
          .willReturn(notFound())
      }

      latestCommitFinder.findLatestCommit(projectId, maybeAccessToken = None).value.unsafeRunSync() shouldBe None
    }

    "return an UnauthorizedException if remote client responds with UNAUTHORIZED" in new TestCase {

      stubFor {
        get(s"/api/v4/projects/$projectId/repository/commits?per_page=1")
          .willReturn(unauthorized())
      }

      intercept[Exception] {
        latestCommitFinder.findLatestCommit(projectId, maybeAccessToken = None).value.unsafeRunSync()
      } shouldBe UnauthorizedException
    }

    "return an Exception if remote client responds with status neither OK nor UNAUTHORIZED" in new TestCase {

      stubFor {
        get(s"/api/v4/projects/$projectId/repository/commits?per_page=1")
          .willReturn(badRequest().withBody("some error"))
      }

      intercept[Exception] {
        latestCommitFinder.findLatestCommit(projectId, maybeAccessToken = None).value.unsafeRunSync()
      }.getMessage shouldBe s"GET $gitLabUrl/api/v4/projects/$projectId/repository/commits?per_page=1 returned ${Status.BadRequest}; body: some error"
    }

    "return an Exception if remote client responds with unexpected body" in new TestCase {

      stubFor {
        get(s"/api/v4/projects/$projectId/repository/commits?per_page=1")
          .willReturn(okJson("{}"))
      }

      intercept[Exception] {
        latestCommitFinder.findLatestCommit(projectId, maybeAccessToken = None).value.unsafeRunSync()
      }.getMessage shouldBe s"GET $gitLabUrl/api/v4/projects/$projectId/repository/commits?per_page=1 returned ${Status.Ok}; error: Invalid message body: Could not decode JSON: {}"
    }
  }

  private implicit val cs:    ContextShift[IO] = IO.contextShift(global)
  private implicit val timer: Timer[IO]        = IO.timer(global)

  private trait TestCase {
    val gitLabUrl   = GitLabUrl(externalServiceBaseUrl)
    val projectId   = projectIds.generateOne
    val accessToken = accessTokens.generateOne
    val commitInfo  = commitInfos.generateOne

    val latestCommitFinder = new IOLatestCommitFinder(gitLabUrl, Throttler.noThrottling, TestLogger())
  }

  private def commitsJson(from: CommitInfo) =
    Json.arr(commitJson(from)).noSpaces

  private def commitJson(commitInfo: CommitInfo) = json"""{
    "id":              ${commitInfo.id.value},
    "author_name":     ${commitInfo.author.name.value},
    "author_email":    ${commitInfo.author.emailToJson},
    "committer_name":  ${commitInfo.committer.name.value},
    "committer_email": ${commitInfo.committer.emailToJson},
    "message":         ${commitInfo.message.value},
    "committed_date":  ${commitInfo.committedDate.value},
    "parent_ids":      ${commitInfo.parents.map(_.value).toArray}
  }"""
}
