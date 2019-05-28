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

import java.util.concurrent.{ConcurrentHashMap, ConcurrentLinkedQueue}

import cats.effect._
import ch.datascience.db.DbTransactor
import ch.datascience.dbeventlog.EventLogDB
import ch.datascience.dbeventlog.commands.IOEventLogFetch
import ch.datascience.generators.Generators.Implicits._
import ch.datascience.graph.model.events.EventsGenerators._
import ch.datascience.graph.model.events.SerializedCommitEvent
import ch.datascience.interpreters.TestLogger
import ch.datascience.interpreters.TestLogger.Level.Info
import com.typesafe.config.ConfigFactory
import doobie.util.transactor.Transactor
import org.scalacheck.Gen.listOfN
import org.scalamock.scalatest.MockFactory
import org.scalatest.Matchers._
import org.scalatest.WordSpec
import org.scalatest.concurrent.{Eventually, IntegrationPatience}

import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.{higherKinds, postfixOps, reflectiveCalls}

class DbEventProcessorRunnerSpec extends WordSpec with Eventually with IntegrationPatience with MockFactory {

  "event source" should {

    "send every new event fetched from db to the registered processor and process them concurrently" in new TestCase {
      val events = listOfN(1000, serializedCommitEvents).generateOne

      eventLogFetch.addEventsToReturn(events)

      val accumulator = new ConcurrentHashMap[SerializedCommitEvent, Long]()
      def processor(event: SerializedCommitEvent): IO[Unit] = {
        accumulator.put(event, Thread.currentThread().getId)
        IO.unit
      }

      eventSourceWith(processor).run.unsafeRunAsyncAndForget()

      eventually {
        accumulator.keySet().asScala shouldBe events.toSet
      }

      val subsequentEvent = serializedCommitEvents.generateOne
      eventLogFetch.addEventsToReturn(Seq(subsequentEvent))

      eventually {
        accumulator.keySet().asScala shouldBe (events :+ subsequentEvent).toSet
      }

      logger.loggedOnly(Info("Waiting for new events"))
    }

    "continue if there is an error during processing" in new TestCase {
      val serializedEvent1 = serializedCommitEvents.generateOne
      val serializedEvent2 = serializedCommitEvents.generateOne
      val serializedEvent3 = serializedCommitEvents.generateOne

      eventLogFetch.addEventsToReturn(Seq(serializedEvent1, serializedEvent2, serializedEvent3))

      val accumulator = new ConcurrentHashMap[SerializedCommitEvent, Long]()
      def processor(event: SerializedCommitEvent): IO[Unit] =
        if (event == serializedEvent2)
          IO.raiseError(new Exception("error during processing serializedEvent2"))
        else {
          accumulator.put(event, Thread.currentThread().getId)
          IO.unit
        }

      eventSourceWith(processor).run.unsafeRunAsyncAndForget()

      eventually {
        accumulator.keySet().asScala shouldBe Set(serializedEvent1, serializedEvent3)
      }

      val serializedEvent4 = serializedCommitEvents.generateOne
      eventLogFetch.addEventsToReturn(Seq(serializedEvent4))

      eventually {
        accumulator.keySet().asScala shouldBe Set(serializedEvent1, serializedEvent3, serializedEvent4)
      }

      logger.loggedOnly(Info("Waiting for new events"))
    }
  }

  private implicit val contextShift: ContextShift[IO] = IO.contextShift(ExecutionContext.global)
  private implicit val timer:        Timer[IO]        = IO.timer(ExecutionContext.global)

  private trait TestCase {
    class TestDbTransactor(transactor: Transactor.Aux[IO, _]) extends DbTransactor[IO, EventLogDB](transactor)
    private val transactor = mock[TestDbTransactor]
    val eventLogFetch = new IOEventLogFetch(transactor) {
      private val eventsQueue = new ConcurrentLinkedQueue[SerializedCommitEvent]()

      def addEventsToReturn(events: Seq[SerializedCommitEvent]): Unit =
        eventsQueue addAll events.asJava

      override def popEventToProcess: IO[Option[SerializedCommitEvent]] = IO.pure {
        Option(eventsQueue.poll())
      }

      override def isEventToProcess: IO[Boolean] = IO.pure {
        !eventsQueue.isEmpty
      }
    }

    val logger               = TestLogger[IO]()
    private val config       = ConfigFactory.parseMap(Map("generation-processes-number" -> 5).asJava)
    private val eventRunner  = DbEventProcessorRunner(_, eventLogFetch, config, logger)
    private val eventsSource = new EventsSource[IO](eventRunner)

    def eventSourceWith(processor: EventProcessor[IO]): EventProcessorRunner[IO] =
      eventsSource.withEventsProcessor(processor).unsafeRunSync()
  }
}
