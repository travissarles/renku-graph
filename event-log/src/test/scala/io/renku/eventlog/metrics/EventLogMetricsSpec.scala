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

package io.renku.eventlog.metrics

import java.lang.Thread.sleep

import cats.effect.{ContextShift, IO, Timer}
import cats.implicits._
import ch.datascience.generators.Generators.Implicits._
import ch.datascience.generators.Generators._
import ch.datascience.interpreters.TestLogger
import ch.datascience.interpreters.TestLogger.Level.Error
import ch.datascience.metrics.{LabeledGauge, SingleValueGauge}
import io.renku.eventlog.DbEventLogGenerators._
import io.renku.eventlog.EventStatus
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.Matchers._
import org.scalatest.WordSpec
import org.scalatest.concurrent.{Eventually, IntegrationPatience}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.language.{higherKinds, postfixOps}

class EventLogMetricsSpec extends WordSpec with MockFactory with Eventually with IntegrationPatience {

  "run" should {

    "update the gauges with the fetched values" in new TestCase {

      val statuses = statuesGen.generateOne
      (statsFinder.statuses _)
        .expects()
        .returning(statuses.pure[IO])
        .atLeastOnce()

      statuses foreach {
        case (status, count) =>
          (statusesGauge.set _)
            .expects(status -> count.toDouble)
            .returning(IO.unit)
            .atLeastOnce()
      }

      (totalGauge.set _)
        .expects(statuses.valuesIterator.sum)
        .returning(IO.unit)
        .atLeastOnce()

      metrics.run.unsafeRunAsyncAndForget()

      sleep(1000)

      logger.expectNoLogs()
    }

    "log an eventual error and continue collecting the metrics" in new TestCase {
      val exception1 = exceptions.generateOne
      (statsFinder.statuses _)
        .expects()
        .returning(exception1.raiseError[IO, Map[EventStatus, Long]])

      val statuses = statuesGen.generateOne
      (statsFinder.statuses _)
        .expects()
        .returning(statuses.pure[IO])
        .atLeastOnce()

      statuses foreach {
        case (status, count) =>
          (statusesGauge.set _)
            .expects(status -> count.toDouble)
            .returning(IO.unit)
            .atLeastOnce()
      }

      (totalGauge.set _)
        .expects(statuses.valuesIterator.sum)
        .returning(IO.unit)
        .atLeastOnce()

      metrics.run.start.unsafeRunAsyncAndForget()

      sleep(1000)

      eventually {
        logger.loggedOnly(Error("Problem with gathering metrics", exception1))
      }
    }
  }

  private implicit val contextShift: ContextShift[IO] = IO.contextShift(ExecutionContext.global)
  private implicit val timer:        Timer[IO]        = IO.timer(ExecutionContext.global)

  private trait TestGauges {
    lazy val statusesGauge = mock[LabeledGauge[IO, EventStatus]]
    lazy val totalGauge    = mock[SingleValueGauge[IO]]
  }

  private trait TestCase extends TestGauges {
    lazy val statsFinder: StatsFinder[IO] = mock[StatsFinder[IO]]
    lazy val logger = TestLogger[IO]()
    lazy val metrics = new EventLogMetrics(
      statsFinder,
      logger,
      statusesGauge,
      totalGauge,
      interval         = 100 millis,
      statusesInterval = 500 millis
    )
  }

  private lazy val statuesGen: Gen[Map[EventStatus, Long]] = nonEmptySet {
    for {
      status <- eventStatuses
      count  <- nonNegativeLongs()
    } yield status -> count.value
  }.map(_.toMap)
}
