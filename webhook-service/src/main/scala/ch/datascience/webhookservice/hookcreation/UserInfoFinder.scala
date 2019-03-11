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
import ch.datascience.graph.model.events._
import ch.datascience.http.client.{AccessToken, IORestClient}
import ch.datascience.webhookservice.config.GitLabConfigProvider
import ch.datascience.webhookservice.hookcreation.UserInfoFinder.UserInfo

import scala.concurrent.ExecutionContext
import scala.language.higherKinds

private trait UserInfoFinder[Interpretation[_]] {
  def findUserInfo(
      userId:      UserId,
      accessToken: AccessToken
  ): Interpretation[UserInfo]
}

private object UserInfoFinder {

  final case class UserInfo(
      userId:   UserId,
      username: Username
  )
}

private class IOUserInfoFinder(
    gitLabConfigProvider:    GitLabConfigProvider[IO]
)(implicit executionContext: ExecutionContext, contextShift: ContextShift[IO])
    extends IORestClient
    with UserInfoFinder[IO] {

  import cats.effect._
  import ch.datascience.http.client.RestClientError.UnauthorizedException
  import io.circe._
  import org.http4s.Method.GET
  import org.http4s.Status.Unauthorized
  import org.http4s._
  import org.http4s.circe._
  import org.http4s.dsl.io._

  def findUserInfo(userId: UserId, accessToken: AccessToken): IO[UserInfo] =
    for {
      gitLabHostUrl <- gitLabConfigProvider.get
      uri           <- validateUri(s"$gitLabHostUrl/api/v4/users/$userId")
      userInfo      <- send(request(GET, uri, accessToken))(mapResponse)
    } yield userInfo

  private lazy val mapResponse: PartialFunction[(Status, Request[IO], Response[IO]), IO[UserInfo]] = {
    case (Ok, _, response)    => response.as[UserInfo]
    case (Unauthorized, _, _) => IO.raiseError(UnauthorizedException)
  }

  private implicit lazy val userInfoDecoder: EntityDecoder[IO, UserInfo] = {
    implicit val hookNameDecoder: Decoder[UserInfo] = (cursor: HCursor) =>
      for {
        id       <- cursor.downField("id").as[UserId]
        username <- cursor.downField("username").as[Username]
      } yield UserInfo(id, username)

    jsonOf[IO, UserInfo]
  }
}