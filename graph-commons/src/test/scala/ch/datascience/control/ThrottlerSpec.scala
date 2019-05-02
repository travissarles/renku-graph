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

package ch.datascience.control

import java.util.concurrent.ConcurrentHashMap

import cats.MonadError
import cats.effect._
import cats.implicits._
import eu.timepit.refined.auto._
import org.scalatest.Matchers._
import org.scalatest.WordSpec

import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.language.postfixOps

class ThrottlerSpec extends WordSpec {

  "Throttler" should {

    "enforce processing with throughput not greater than demanded" in new TestCase {

      val startTime = {
        for {
          throttler <- Throttler[IO, ThrottlingTarget](RateLimit(10L, per = 1 second))
          startTime <- timer.clock.monotonic(MILLISECONDS)
          _         <- (1 to 20).map(useThrottledResource(_, throttler)).toList.parSequence
        } yield startTime
      }.unsafeRunSync()

      val startDelays = register.asScala.values
        .map(greenLight => greenLight - startTime)
        .toList
        .sorted
        .foldLeft(List.empty[Long]) {
          case (diffs, item) => diffs :+ item - diffs.sum
        }
      startDelays.tail foreach { delay =>
        delay should be >= (100L - (100L * 0.30).toLong)
      }

      val totalTime = register.asScala.values
        .map(greenLight => greenLight - startTime)
        .sum
      totalTime should be > (startDelays.size * 100L)
    }

    "not sequence work items but process them in parallel" in new TestCase {

      val startTime = {
        for {
          throttler <- Throttler[IO, ThrottlingTarget](RateLimit(200L, per = 1 second))
          startTime <- timer.clock.monotonic(MILLISECONDS)
          _         <- (1 to 20).map(useThrottledResource(_, throttler, processingTime = 500 millis)).toList.parSequence
        } yield startTime
      }.unsafeRunSync()

      val startDelays = register.asScala.values
        .map(greenLight => greenLight - startTime)
        .toList
        .sorted
        .foldLeft(List.empty[Long]) {
          case (diffs, item) => diffs :+ item - diffs.sum
        }
      startDelays foreach { delay =>
        delay should be < 1000L
      }

      val totalTime = register.asScala.values
        .map(greenLight => greenLight - startTime)
        .sum
      totalTime should be < (startDelays.size * 500L)
    }
  }

  "noThrottling" should {

    "return Throttler which does not do anything to the throughput" in new TestCase {
      val throttler = Throttler.noThrottling[IO, Any]

      val startTime = {
        for {
          startTime <- timer.clock.monotonic(MILLISECONDS)
          _         <- (1 to 20).map(useThrottledResource(_, throttler)).toList.parSequence
        } yield startTime
      }.unsafeRunSync()

      val startDelays = register.asScala.values
        .map(greenLight => greenLight - startTime)
        .toList
        .sorted
        .foldLeft(List.empty[Long]) {
          case (Nil, item)   => List(item)
          case (diffs, item) => diffs :+ item - diffs.sum
        }
      startDelays foreach { delay =>
        delay should be < 1000L
      }

      val totalTime = register.asScala.values
        .map(greenLight => greenLight - startTime)
        .sum
      totalTime should be < (5 * 500L)
    }
  }

  private implicit val contextShift: ContextShift[IO] = IO.contextShift(ExecutionContext.global)
  private implicit val timer:        Timer[IO]        = IO.timer(ExecutionContext.global)
  private val clock:                 Clock[IO]        = timer.clock

  private trait TestCase {

    val context  = MonadError[IO, Throwable]
    val register = new ConcurrentHashMap[String, Long]()

    def useThrottledResource[Target](name:           Int,
                                     throttler:      Throttler[IO, Target],
                                     processingTime: FiniteDuration = 0 seconds): IO[Unit] =
      for {
        _          <- throttler.acquire
        greenLight <- clock.monotonic(MILLISECONDS)
        _          <- context.pure(register.put(name.toString, greenLight))
        _          <- timer.sleep(processingTime)
        _          <- throttler.release
      } yield ()
  }

  private trait ThrottlingTarget
}