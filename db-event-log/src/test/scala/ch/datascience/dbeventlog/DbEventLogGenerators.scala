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

package ch.datascience.dbeventlog

import EventStatus._
import cats.implicits._
import ch.datascience.dbeventlog.commands.ProcessingStatus
import ch.datascience.generators.Generators._
import org.scalacheck.Gen

import scala.util.Try

object DbEventLogGenerators {

  implicit val createdDates:   Gen[CreatedDate]   = timestampsNotInTheFuture map CreatedDate.apply
  implicit val executionDates: Gen[ExecutionDate] = timestamps map ExecutionDate.apply
  implicit val eventStatuses: Gen[EventStatus] = Gen.oneOf(
    New,
    Processing,
    TriplesStore,
    TriplesStoreFailure,
    NonRecoverableFailure
  )
  implicit val eventMessages: Gen[EventMessage] = nonEmptyStrings() map EventMessage.apply
  implicit val processingStatuses: Gen[ProcessingStatus] =
    for {
      total <- positiveInts(max = Integer.MAX_VALUE)
      done  <- positiveInts(max = total.value)
    } yield ProcessingStatus.from[Try](done.value, total.value).fold(throw _, identity)
}
