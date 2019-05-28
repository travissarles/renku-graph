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

package ch.datascience.dbeventlog.commands

import java.time.Instant

import ch.datascience.dbeventlog.{EventStatus, ExecutionDate}
import EventStatus._
import ch.datascience.generators.Generators.Implicits._
import ch.datascience.generators.Generators._
import ch.datascience.graph.model.events.CommitEventId
import ch.datascience.graph.model.events.EventsGenerators._
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.Matchers._
import org.scalatest.WordSpec

class EventLogMarkAllNewSpec extends WordSpec with InMemoryEventLogDbSpec with MockFactory {

  "markAllEventsAsNew" should {

    s"set status to $New and execution_date to now " +
      s"on all events with status $Processing, $TriplesStore and $TriplesStoreFailure " +
      "and execution_date in the past" in new TestCase {

      val event1Id = commitEventIds.generateOne
      addEvent(event1Id, EventStatus.Processing, timestampsNotInTheFuture.map(ExecutionDate.apply))
      val event2Id = commitEventIds.generateOne
      addEvent(event2Id, EventStatus.Processing, timestampsInTheFuture.map(ExecutionDate.apply))
      val event3Id = commitEventIds.generateOne
      addEvent(event3Id, EventStatus.TriplesStore, timestampsNotInTheFuture.map(ExecutionDate.apply))
      val event4Id = commitEventIds.generateOne
      addEvent(event4Id, EventStatus.NonRecoverableFailure, timestampsNotInTheFuture.map(ExecutionDate.apply))
      val event5Id = commitEventIds.generateOne
      addEvent(event5Id, EventStatus.New, timestampsNotInTheFuture.map(ExecutionDate.apply))
      val event6Id = commitEventIds.generateOne
      addEvent(event6Id, EventStatus.TriplesStoreFailure, timestampsNotInTheFuture.map(ExecutionDate.apply))

      eventLogMarkAllNew.markAllEventsAsNew.unsafeRunSync() shouldBe ((): Unit)

      findEvents(status = New).toSet shouldBe Set(event1Id -> ExecutionDate(now),
                                                  event3Id -> ExecutionDate(now),
                                                  event5Id -> ExecutionDate(now),
                                                  event6Id -> ExecutionDate(now))
    }

    "do nothing when no events got updated" in new TestCase {

      val event1Id = commitEventIds.generateOne
      addEvent(event1Id, EventStatus.Processing, timestampsInTheFuture.map(ExecutionDate.apply))

      eventLogMarkAllNew.markAllEventsAsNew.unsafeRunSync() shouldBe ((): Unit)

      findEvents(status = New) shouldBe Nil
    }
  }

  private trait TestCase {

    val currentTime        = mockFunction[Instant]
    val eventLogMarkAllNew = new EventLogMarkAllNew(transactor, currentTime)

    val now = Instant.now()
    currentTime.expects().returning(now).anyNumberOfTimes()

    def addEvent(commitEventId: CommitEventId, status: EventStatus, executionDate: Gen[ExecutionDate]): Unit =
      storeEvent(commitEventId,
                 status,
                 executionDate.generateOne,
                 committedDates.generateOne,
                 serializedCommitEvents.generateOne)
  }
}