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

package ch.datascience.triplesgenerator.eventprocessing

import cats.effect.{ContextShift, IO}
import cats.implicits._
import ch.datascience.config.ServiceUrl
import ch.datascience.triplesgenerator.config.FusekiConfigProvider
import org.apache.jena.rdfconnection.{RDFConnection, RDFConnectionFuseki}

import scala.language.higherKinds
import scala.util.control.NonFatal

private abstract class FusekiConnector[Interpretation[_]] {
  def upload(rdfTriples: RDFTriples): Interpretation[Unit]
}

private class IOFusekiConnector(
    fusekiConfigProvider:    FusekiConfigProvider[IO],
    fusekiConnectionBuilder: ServiceUrl => RDFConnection = IOFusekiConnector.fusekiConnectionBuilder
)(implicit contextShift:     ContextShift[IO])
    extends FusekiConnector[IO] {

  def upload(rdfTriples: RDFTriples): IO[Unit] =
    newConnection
      .bracket(send(rdfTriples))(closeConnection)
      .recoverWith(meaningfulError)

  private def newConnection: IO[RDFConnection] =
    contextShift.shift *> fusekiConfigProvider.get.flatMap { fusekiConfig =>
      IO(fusekiConnectionBuilder(fusekiConfig.fusekiBaseUrl / fusekiConfig.datasetName))
    }

  private def send(rdfTriples: RDFTriples)(connection: RDFConnection): IO[Unit] =
    IO(connection.load(rdfTriples.value))

  private def closeConnection(connection: RDFConnection): IO[Unit] =
    IO(connection.close())

  private lazy val meaningfulError: PartialFunction[Throwable, IO[Unit]] = {
    case NonFatal(exception) =>
      IO.raiseError(new RuntimeException("Uploading triples to Jena failed", exception))
  }
}

private object IOFusekiConnector {
  private val fusekiConnectionBuilder: ServiceUrl => RDFConnection = fusekiUrl =>
    RDFConnectionFuseki
      .create()
      .destination(fusekiUrl.toString)
      .build()
}