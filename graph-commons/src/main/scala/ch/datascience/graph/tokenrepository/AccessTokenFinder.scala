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

package ch.datascience.graph.tokenrepository

import cats.effect.{ContextShift, IO, Timer}
import ch.datascience.control.Throttler
import ch.datascience.graph.model.projects.{Id, Path}
import ch.datascience.http.client.{AccessToken, IORestClient}
import io.chrisdavenport.log4cats.Logger

import scala.concurrent.ExecutionContext
import scala.language.higherKinds

trait AccessTokenFinder[Interpretation[_]] {
  def findAccessToken[ID](projectId: ID)(implicit toPathSegment: ID => String): Interpretation[Option[AccessToken]]
}

class IOAccessTokenFinder(
    tokenRepositoryUrl:      TokenRepositoryUrl,
    logger:                  Logger[IO]
)(implicit executionContext: ExecutionContext, contextShift: ContextShift[IO], timer: Timer[IO])
    extends IORestClient(Throttler.noThrottling, logger)
    with AccessTokenFinder[IO] {

  import cats.effect._
  import org.http4s.Method.GET
  import org.http4s._
  import org.http4s.circe._
  import org.http4s.dsl.io._

  def findAccessToken[ID](projectId: ID)(implicit toPathSegment: ID => String): IO[Option[AccessToken]] =
    for {
      uri         <- validateUri(s"$tokenRepositoryUrl/projects/${toPathSegment(projectId)}/tokens")
      accessToken <- send(request(GET, uri))(mapResponse)
    } yield accessToken

  private lazy val mapResponse: PartialFunction[(Status, Request[IO], Response[IO]), IO[Option[AccessToken]]] = {
    case (Ok, _, response) => response.as[Option[AccessToken]]
    case (NotFound, _, _)  => IO.pure(None)
  }

  private implicit lazy val accessTokenEntityDecoder: EntityDecoder[IO, Option[AccessToken]] = {
    jsonOf[IO, AccessToken].map(Option.apply)
  }
}

object IOAccessTokenFinder {
  import ch.datascience.http.client.UrlEncoder.urlEncode
  implicit val projectPathToPath: Path => String = path => urlEncode(path.value)
  implicit val projectIdToPath:   Id => String   = _.toString

  def apply(
      logger:                  Logger[IO]
  )(implicit executionContext: ExecutionContext,
    contextShift:              ContextShift[IO],
    timer:                     Timer[IO]): IO[AccessTokenFinder[IO]] =
    for {
      tokenRepositoryUrl <- TokenRepositoryUrl[IO]()
    } yield new IOAccessTokenFinder(tokenRepositoryUrl, logger)
}
