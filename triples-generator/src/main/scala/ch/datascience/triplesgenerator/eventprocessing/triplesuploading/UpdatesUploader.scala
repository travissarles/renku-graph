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

package ch.datascience.triplesgenerator.eventprocessing.triplesuploading

import cats.MonadError
import cats.effect.{ContextShift, IO, Timer}
import ch.datascience.http.client.IORestClient.{MaxRetriesAfterConnectionTimeout, SleepAfterConnectionIssue}
import ch.datascience.rdfstore.{IORdfStoreClient, RdfStoreConfig, SparqlQueryTimeRecorder}
import ch.datascience.triplesgenerator.eventprocessing.triplescuration.CuratedTriples.Update
import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.NonNegative
import io.chrisdavenport.log4cats.Logger

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration
import scala.language.higherKinds

private trait UpdatesUploader[Interpretation[_]] {
  def send(updates: List[Update]): Interpretation[TriplesUploadResult]
}

private class IOUpdatesUploader(
    rdfStoreConfig:          RdfStoreConfig,
    logger:                  Logger[IO],
    timeRecorder:            SparqlQueryTimeRecorder[IO],
    retryInterval:           FiniteDuration = SleepAfterConnectionIssue,
    maxRetries:              Int Refined NonNegative = MaxRetriesAfterConnectionTimeout
)(implicit executionContext: ExecutionContext,
  contextShift:              ContextShift[IO],
  timer:                     Timer[IO],
  ME:                        MonadError[IO, Throwable])
    extends IORdfStoreClient(rdfStoreConfig, logger, timeRecorder, retryInterval, maxRetries)
    with UpdatesUploader[IO] {

  import LogMessage._
  import TriplesUploadResult._
  import cats.implicits._
  import org.http4s.Status.{BadRequest, Ok}
  import org.http4s.{Request, Response, Status}

  import scala.util.control.NonFatal

  override def send(updates: List[Update]): IO[TriplesUploadResult] =
    updates
      .map(update => updateWitMapping(update.query, responseMapper(update)))
      .sequence
      .map(mergeResults)
      .recoverWith(deliveryFailure)

  private def responseMapper(
      update: Update
  ): PartialFunction[(Status, Request[IO], Response[IO]), IO[TriplesUploadResult]] = {
    case (Ok, _, _)                => IO.pure(DeliverySuccess)
    case (BadRequest, _, response) => response.as[String] map toSingleLine map toInvalidUpdatesFailure(update)
    case (other, _, response)      => response.as[String] map toSingleLine map toDeliveryFailure(other)
  }

  private def toInvalidUpdatesFailure(update: Update)(responseMessage: String) =
    InvalidUpdatesFailure(s"Triples curation update '${update.name}' failed: $responseMessage")

  private def toDeliveryFailure(status: Status)(message: String) =
    DeliveryFailure(s"Triples curation update failed: $status: $message")

  private def deliveryFailure: PartialFunction[Throwable, IO[TriplesUploadResult]] = {
    case NonFatal(exception) => ME.pure(DeliveryFailure(exception.getMessage))
  }

  private def mergeResults(results: List[TriplesUploadResult]): TriplesUploadResult =
    results.filterNot(_ == DeliverySuccess) match {
      case Nil => DeliverySuccess
      case failures =>
        failures.partition {
          case _: DeliveryFailure => true
          case _ => false
        } match {
          case (deliveryFailure +: _, _) => deliveryFailure
          case (Nil, otherFailures)      => InvalidUpdatesFailure(otherFailures.map(_.message).mkString("; "))
        }
    }
}
