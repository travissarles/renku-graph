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

package ch.datascience.webhookservice.audit

import java.time.Instant

import cats.data.{NonEmptyList, OptionT}
import cats.effect.IO
import ch.datascience.generators.Generators.Implicits._
import ch.datascience.generators.Generators._
import ch.datascience.graph.model.events.EventsGenerators._
import ch.datascience.graph.model.events.SerializedCommitEvent
import ch.datascience.interpreters.TestLogger
import ch.datascience.interpreters.TestLogger.Level.Info
import ch.datascience.webhookservice.audit.AuditLogConfig.Topic
import ch.datascience.webhookservice.audit.IOAuditLog.EventLog
import ch.epfl.dedis.byzcoin.{InstanceId, SignerCounters}
import ch.epfl.dedis.eventlog.{Event, SearchResponse}
import ch.epfl.dedis.lib.darc.Signer
import ch.epfl.dedis.lib.proto.EventLogProto
import org.scalacheck.Gen
import org.scalamock.matchers.MockParameter
import org.scalamock.scalatest.MockFactory
import org.scalatest.Matchers._
import org.scalatest.WordSpec

import scala.collection.JavaConverters._

class IOAuditLogSpec extends WordSpec with MockFactory {

  "apply" should {

    "return NoAuditLog if Audit Log is disabled in the config" in {
      val logger = TestLogger[IO]()

      IOAuditLog(OptionT.none[IO, AuditLogConfig], logger).unsafeRunSync() shouldBe AuditLogPassThrough

      logger.loggedOnly(Info("SecureKG auditing disabled"))
    }
  }

  "push" should {

    "succeed when passing an event " +
      "with the serialized Commit Event signers and incremented signer counters to the EventLogInstance " +
      "succeeds" in new TestCase {

      (signerCounters.increment _)
        .expects()
        .returning(())

      val counters = countersList.generateOne
      (signerCounters.getCounters _)
        .expects()
        .returning(counters)

      val serializedCommitEvent = serializedCommitEvents.generateOne
      (eventLog
        .log(_: Event, _: java.util.List[Signer], _: java.util.List[java.lang.Long]))
        .expects(eventWith(config.topic, serializedCommitEvent), List(signer).asJava, counters)
        .returning(instanceIds.generateOne)

      auditLog.push(serializedCommitEvent).unsafeRunSync() shouldBe ((): Unit)
    }

    "fail if passing an event to the EventLogInstance fails" in new TestCase {

      (signerCounters.increment _)
        .expects()
        .returning(())

      val counters = countersList.generateOne
      (signerCounters.getCounters _)
        .expects()
        .returning(counters)

      val serializedCommitEvent = serializedCommitEvents.generateOne
      val exception             = exceptions.generateOne
      (eventLog
        .log(_: Event, _: java.util.List[Signer], _: java.util.List[java.lang.Long]))
        .expects(eventWith(config.topic, serializedCommitEvent), List(signer).asJava, counters)
        .throwing(exception)

      intercept[Exception] {
        auditLog.push(serializedCommitEvent).unsafeRunSync()
      } shouldBe exception
    }
  }

  "fetchAllEvents" should {

    "search for events with the defined topic pushed until now" in new TestCase {

      val currentTime = Instant.now()
      now.expects().returning(currentTime)

      val commitEvents = nonEmptyList(serializedCommitEvents).generateOne
      (eventLog
        .search(_: String, _: Long, _: Long))
        .expects(config.topic.value, Instant.EPOCH.toEpochMilli * 1000 * 1000, currentTime.toEpochMilli * 1000 * 1000)
        .returning(searchResponse(commitEvents))

      auditLog.fetchAllEvents.unsafeRunSync() shouldBe commitEvents.toList
    }

    "fail if found events' contents are invalid" in new TestCase {

      val currentTime = Instant.now()
      now.expects().returning(currentTime)

      (eventLog
        .search(_: String, _: Long, _: Long))
        .expects(config.topic.value, Instant.EPOCH.toEpochMilli * 1000 * 1000, currentTime.toEpochMilli * 1000 * 1000)
        .returning(searchResponse(List("")))

      intercept[IllegalArgumentException] {
        auditLog.fetchAllEvents.unsafeRunSync()
      }
    }

    "fail if searching for events fails" in new TestCase {

      val currentTime = Instant.now()
      now.expects().returning(currentTime)

      val exception = exceptions.generateOne
      (eventLog
        .search(_: String, _: Long, _: Long))
        .expects(config.topic.value, Instant.EPOCH.toEpochMilli * 1000 * 1000, currentTime.toEpochMilli * 1000 * 1000)
        .throwing(exception)

      intercept[Exception] {
        auditLog.fetchAllEvents.unsafeRunSync()
      } shouldBe exception
    }
  }

  private trait TestCase {
    val config         = auditLogConfigs.generateOne
    val eventLog       = mock[EventLog]
    val signer         = mock[Signer]
    val signerCounters = mock[SignerCounters]
    val now            = mockFunction[Instant]
    val auditLog       = new IOAuditLog(config, eventLog, signer, signerCounters, now)

    def eventWith(topic: Topic, content: SerializedCommitEvent): MockParameter[Event] = argAssert { event: Event =>
      event.getTopic   shouldBe topic.value
      event.getContent shouldBe content.value
    }

    def searchResponse(serializedCommitEvents: NonEmptyList[SerializedCommitEvent]): SearchResponse =
      searchResponse(serializedCommitEvents.toList map (_.value))

    def searchResponse(eventsContents: List[String]): SearchResponse = {

      val events = eventsContents map { eventContent =>
        val builder: EventLogProto.Event.Builder = EventLogProto.Event.newBuilder()
        builder.setWhen(timestampsNotInTheFuture.map(_.toEpochMilli).generateOne)
        builder.setTopic(config.topic.value)
        builder.setContent(eventContent)
        builder.build()
      }

      new SearchResponse(
        EventLogProto.SearchResponse
          .newBuilder()
          .addAllEvents(events.asJava)
          .setTruncated(true)
          .build()
      )
    }
  }

  private val instanceIds: Gen[InstanceId] =
    Gen
      .listOfN(32, Gen.choose(Byte.MinValue, Byte.MaxValue))
      .map(_.toArray)
      .map(new InstanceId(_))

  private val countersList: Gen[java.util.List[java.lang.Long]] =
    Gen.nonEmptyListOf(Gen.choose(0L, Long.MaxValue)).generateOne.map(new java.lang.Long(_)).asJava

}
