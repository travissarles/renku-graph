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

package ch.datascience.http.server
import cats.data.Kleisli
import cats.effect.IO
import io.circe.Json
import org.http4s.circe.{jsonEncoderOf, jsonOf}
import org.http4s.{EntityDecoder, EntityEncoder, HttpRoutes, Request, Response, Status}

object EndpointTester {

  implicit val jsonEntityDecoder: EntityDecoder[IO, Json] = jsonOf[IO, Json]
  implicit val jsonEntityEncoder: EntityEncoder[IO, Json] = jsonEncoderOf[IO, Json]

  implicit class EndpointOps(endpoint: Kleisli[IO, Request[IO], Response[IO]]) {

    def call(request: Request[IO]) = new {
      private val runResponse: Response[IO] = endpoint.run(request).unsafeRunSync()

      lazy val status: Status = runResponse.status

      def body[T](implicit decoder: EntityDecoder[IO, T]): T = runResponse.as[T].unsafeRunSync
    }
  }

  val notAvailableResponse: Response[IO] = Response(Status.ServiceUnavailable)

  implicit class RoutesOps(routes: HttpRoutes[IO]) {
    def or(response: Response[IO]): Kleisli[IO, Request[IO], Response[IO]] =
      Kleisli(a => routes.run(a).getOrElse(response))
  }
}