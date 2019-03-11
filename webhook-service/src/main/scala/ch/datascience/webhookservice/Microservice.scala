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

package ch.datascience.webhookservice

import cats.effect._
import ch.datascience.http.server.HttpServer
import ch.datascience.webhookservice.eventprocessing.IOHookEventEndpoint
import ch.datascience.webhookservice.hookcreation.IOHookCreationEndpoint
import ch.datascience.webhookservice.hookvalidation.IOHookValidationEndpoint

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}
import scala.language.higherKinds

object Microservice extends IOApp {

  private implicit val executionContext: ExecutionContextExecutor = ExecutionContext.global

  private val httpServer = new HttpServer[IO](
    serverPort = 9001,
    serviceRoutes = new MicroserviceRoutes[IO](
      new IOHookEventEndpoint,
      new IOHookCreationEndpoint,
      new IOHookValidationEndpoint
    ).routes
  )

  override def run(args: List[String]): IO[ExitCode] =
    new MicroserviceRunner(httpServer).run(args)
}

class MicroserviceRunner(httpServer: HttpServer[IO]) {

  def run(args: List[String]): IO[ExitCode] =
    for {
      result <- httpServer.run
    } yield result
}