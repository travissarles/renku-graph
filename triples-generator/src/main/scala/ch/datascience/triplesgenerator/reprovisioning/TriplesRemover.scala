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

package ch.datascience.triplesgenerator.reprovisioning

import cats.effect.{ContextShift, IO, Timer}
import ch.datascience.rdfstore._
import io.chrisdavenport.log4cats.Logger

import scala.concurrent.ExecutionContext
import scala.language.higherKinds

private trait TriplesRemover[Interpretation[_]] {
  def removeAllTriples(): Interpretation[Unit]
}

private object TriplesRemover {
  val TriplesRemovalBatchSize: Long = 50000
}

private class IOTriplesRemover(
    rdfStoreConfig:          RdfStoreConfig,
    logger:                  Logger[IO],
    timeRecorder:            SparqlQueryTimeRecorder[IO]
)(implicit executionContext: ExecutionContext, contextShift: ContextShift[IO], timer: Timer[IO])
    extends IORdfStoreClient(rdfStoreConfig, logger, timeRecorder)
    with TriplesRemover[IO] {

  import TriplesRemover._
  import cats.implicits._
  import eu.timepit.refined.auto._
  import io.circe.Decoder

  import scala.util.Try

  override def removeAllTriples(): IO[Unit] =
    queryExpecting[Long](findTriplesCount)(countDecoder) flatMap {
      case 0 => IO.unit
      case _ =>
        for {
          _ <- updateWitNoResult(removeTriplesBatch)
          _ <- removeAllTriples()
        } yield ()
    }

  private val findTriplesCount = SparqlQuery(
    name     = "triples remove - count",
    prefixes = Set.empty,
    """|SELECT (COUNT(*) AS ?count)
       |WHERE { ?s ?p ?o }
       |""".stripMargin
  )

  private val removeTriplesBatch = SparqlQuery(
    name     = "triples remove - delete",
    prefixes = Set.empty,
    s"""|DELETE { ?s ?p ?o }
        |WHERE { 
        |  SELECT  ?s ?p ?o 
        |  WHERE { ?s ?p ?o }
        |  LIMIT $TriplesRemovalBatchSize
        |}
        |""".stripMargin
  )

  private implicit val countDecoder: Decoder[Long] = {
    import io.circe.Decoder.decodeList
    import io.circe.DecodingFailure

    val rows: Decoder[Long] = _.downField("count")
      .downField("value")
      .as[String]
      .flatMap { count =>
        Try(count.toLong).toEither.leftMap { ex =>
          DecodingFailure(s"Triples count in non-number format: $ex", Nil)
        }
      }

    _.downField("results")
      .downField("bindings")
      .as[List[Long]](decodeList(rows))
      .map(_.headOption.getOrElse(0))
  }
}
