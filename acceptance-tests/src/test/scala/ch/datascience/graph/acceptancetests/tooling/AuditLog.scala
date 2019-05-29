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

package ch.datascience.graph.acceptancetests.tooling

import cats.effect.{IO, Timer}
import cats.implicits._
import ch.datascience.graph.model.events.CommitId
import ch.datascience.interpreters.TestLogger
import ch.datascience.webhookservice.audit.IOAuditLog
import io.circe.parser.parse
import io.circe.{Decoder, HCursor}

import scala.concurrent.ExecutionContext
import scala.language.postfixOps

object AuditLog {
  import java.util.function.Consumer

  import org.testcontainers.containers.GenericContainer
  import org.testcontainers.containers.wait.strategy.Wait
  import org.testcontainers.images.builder.ImageFromDockerfile
  import org.testcontainers.images.builder.dockerfile.DockerfileBuilder

  import scala.collection.JavaConverters._
  import scala.util.control.NonFatal

  private val ServerImageName          = "dedis/conode-test:latest"
  private val TemporaryDockerImageName = "conode-test-run"

  private val ServerExposedPorts    = (7770 to 7783).toList
  private val PortBindings          = ServerExposedPorts.map(port => s"$port:$port")
  private val ContainerExposedPorts = (7770 to 7777).toList

  private implicit val timer: Timer[IO] = IO.timer(ExecutionContext.global)

  def start(): IO[Unit] =
    IO {
      val dockerImage = new ImageFromDockerfile(TemporaryDockerImageName, true).withDockerfileFromBuilder(dockerBuilder)
      val container   = new GenericContainer(dockerImage)
      container.setPortBindings(PortBindings.asJava)
      container.withExposedPorts(ContainerExposedPorts.map(new java.lang.Integer(_)): _*)
      container.waitingFor(Wait.forListeningPort)
      container.start()
    } recoverWith meaningfulException

  private lazy val dockerBuilder: Consumer[DockerfileBuilder] = (builder: DockerfileBuilder) =>
    builder
      .from(ServerImageName)
      .expose(ServerExposedPorts.map(new java.lang.Integer(_)): _*)

  private lazy val meaningfulException: PartialFunction[Throwable, IO[Unit]] = {
    case NonFatal(exception) =>
      IO.raiseError {
        new IllegalStateException(
          s"Cannot start the Audit Log from '$ServerImageName' Docker image. Please ensure that local conodes are not running.",
          exception)
      }
  }

  def fetchAllEvents: List[CommitId] = {
    for {
      auditLog            <- IOAuditLog(logger = TestLogger[IO]())
      allEventsSerialized <- auditLog.fetchAllEvents
      commitIds           <- allEventsSerialized.map(_.value).map(deserialize).sequence
    } yield commitIds
  }.unsafeRunSync()

  private lazy val deserialize: String => IO[CommitId] =
    json => IO.fromEither(parse(json).flatMap(_.as[CommitId]))

  private implicit val serializedCommitEventsDecoder: Decoder[CommitId] =
    (cursor: HCursor) => cursor.downField("id").as[CommitId]
}
