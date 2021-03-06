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

package ch.datascience.http.client

import java.net.ConnectException

import cats.effect.{ContextShift, IO, Timer}
import ch.datascience.config.ServiceUrl
import ch.datascience.control.Throttler
import ch.datascience.generators.Generators.Implicits._
import ch.datascience.generators.Generators._
import ch.datascience.http.client.RestClientError._
import ch.datascience.interpreters.TestLogger
import ch.datascience.interpreters.TestLogger.Level.Warn
import ch.datascience.logging.{ExecutionTimeRecorder, TestExecutionTimeRecorder}
import ch.datascience.stubbing.ExternalServiceStubbing
import com.github.tomakehurst.wiremock.client.WireMock._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.auto._
import eu.timepit.refined.collection.NonEmpty
import io.chrisdavenport.log4cats.Logger
import io.prometheus.client.Histogram
import org.http4s.Method.GET
import org.http4s.{Request, Response, Status}
import org.scalamock.scalatest.MockFactory
import org.scalatest.Matchers._
import org.scalatest.WordSpec

import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.language.postfixOps

class IORestClientSpec extends WordSpec with ExternalServiceStubbing with MockFactory {

  "send" should {

    "succeed returning value calculated with the given response mapping rules " +
      "if the response matches the rules" in new TestCase {

      stubFor {
        get("/resource")
          .willReturn(ok("1"))
      }

      verifyThrottling()

      client.callRemote.unsafeRunSync() shouldBe 1

      logger.loggedOnly(Warn(s"GET $hostUrl/resource finished${executionTimeRecorder.executionTimeInfo}"))
    }

    "succeed returning value calculated with the given response mapping rules " +
      "and do not measure execution time if Time Recorder not given" in new TestCase {

      stubFor {
        get("/resource")
          .willReturn(ok("1"))
      }

      verifyThrottling()

      override val client = new TestRestClient(hostUrl, throttler, logger, maybeTimeRecorder = None)

      client.callRemote.unsafeRunSync() shouldBe 1

      logger.expectNoLogs()
    }

    "succeed returning value calculated with the given response mapping rules and " +
      "log execution time along with the given request name if Time Recorder present" in new TestCase {

      stubFor {
        get("/resource")
          .willReturn(ok("1"))
      }

      verifyThrottling()

      val requestName: String Refined NonEmpty = "some request"
      client.callRemote(requestName).unsafeRunSync() shouldBe 1

      logger.loggedOnly(Warn(s"$requestName finished${executionTimeRecorder.executionTimeInfo}"))
    }

    "cause the given histogram to capture execution time - case with some given label" in new TestCase {

      stubFor {
        get("/resource")
          .willReturn(ok("1"))
      }

      verifyThrottling()

      val requestName: String Refined NonEmpty = "some request"
      client.callRemote(requestName).unsafeRunSync() shouldBe 1

      val Some(sample) = histogram.collect().asScala.flatMap(_.samples.asScala).lastOption
      sample.value               should be >= 0d
      sample.labelNames.asScala  should contain only histogramLabel.value
      sample.labelValues.asScala should contain only requestName.value
    }

    "cause the given histogram to capture execution time - case without label" in new TestCase {

      stubFor {
        get("/resource")
          .willReturn(ok("1"))
      }

      verifyThrottling()

      override val histogram = Histogram.build("histogram", "help").create()

      client.callRemote.unsafeRunSync() shouldBe 1

      val Some(sample) = histogram.collect().asScala.flatMap(_.samples.asScala).lastOption
      sample.value               should be >= 0d
      sample.labelNames.asScala  shouldBe empty
      sample.labelValues.asScala shouldBe empty
    }

    "fail if remote responds with status which does not match the response mapping rules" in new TestCase {

      stubFor {
        get("/resource")
          .willReturn(
            aResponse
              .withStatus(Status.NotFound.code)
              .withBody("some body")
          )
      }

      verifyThrottling()

      intercept[UnexpectedResponseException] {
        client.callRemote.unsafeRunSync()
      }.getMessage shouldBe s"GET $hostUrl/resource returned ${Status.NotFound}; body: some body"
    }

    "fail if remote responds with an empty body and status which doesn't match the response mapping rules" in new TestCase {

      stubFor {
        get("/resource")
          .willReturn(noContent())
      }

      verifyThrottling()

      intercept[UnexpectedResponseException] {
        client.callRemote.unsafeRunSync()
      }.getMessage shouldBe s"GET $hostUrl/resource returned ${Status.NoContent}; body: "
    }

    "fail if remote responds with a BAD_REQUEST and it's not mapped in the given response mapping rules" in new TestCase {

      val responseBody = nonBlankStrings().generateOne
      stubFor {
        get("/resource")
          .willReturn(aResponse.withStatus(Status.BadRequest.code).withBody(responseBody))
      }

      verifyThrottling()

      intercept[BadRequestException] {
        client.callRemote.unsafeRunSync()
      }.getMessage shouldBe s"GET $hostUrl/resource returned ${Status.BadRequest}; body: $responseBody"
    }

    "fail if remote responds with a body which causes exception during mapping" in new TestCase {

      stubFor {
        get("/resource")
          .willReturn(ok("non int"))
      }

      verifyThrottling()

      val exception = intercept[MappingException] {
        client.callRemote.unsafeRunSync()
      }

      exception.getMessage shouldBe s"""GET $hostUrl/resource returned ${Status.Ok}; error: For input string: "non int""""
      exception.getCause   shouldBe a[NumberFormatException]
    }

    "fail after retrying if there is a persistent connectivity problem" in {
      val logger = TestLogger[IO]()

      val exception = intercept[ConnectivityException] {
        new TestRestClient(ServiceUrl("http://localhost:1024"), Throttler.noThrottling, logger, None).callRemote
          .unsafeRunSync()
      }
      exception.getMessage shouldBe s"GET http://localhost:1024/resource error: Connection refused"
      exception.getCause   shouldBe a[ConnectException]

      logger.loggedOnly(
        Warn("GET http://localhost:1024/resource timed out -> retrying attempt 1 error: Connection refused"),
        Warn("GET http://localhost:1024/resource timed out -> retrying attempt 2 error: Connection refused")
      )
    }
  }

  private trait TestCase {
    val histogramLabel: String Refined NonEmpty = "label"
    val histogram             = Histogram.build("histogram", "help").labelNames(histogramLabel.value).create()
    val throttler             = mock[Throttler[IO, Any]]
    val logger                = TestLogger[IO]()
    val executionTimeRecorder = TestExecutionTimeRecorder[IO](logger, Some(histogram))
    val client                = new TestRestClient(hostUrl, throttler, logger, Some(executionTimeRecorder))

    def verifyThrottling() = inSequence {
      (throttler.acquire _).expects().returning(IO.unit)
      (throttler.release _).expects().returning(IO.unit)
    }
  }

  private implicit val cs:    ContextShift[IO] = IO.contextShift(global)
  private implicit val timer: Timer[IO]        = IO.timer(global)
  private val hostUrl = ServiceUrl(externalServiceBaseUrl)

  private class TestRestClient(hostUrl:           ServiceUrl,
                               throttler:         Throttler[IO, Any],
                               logger:            Logger[IO],
                               maybeTimeRecorder: Option[ExecutionTimeRecorder[IO]])
      extends IORestClient(throttler, logger, maybeTimeRecorder, retryInterval = 1 millisecond, maxRetries = 2) {

    def callRemote: IO[Int] =
      for {
        uri         <- validateUri(s"$hostUrl/resource")
        accessToken <- send(request(GET, uri))(mapResponse)
      } yield accessToken

    def callRemote(requestName: String Refined NonEmpty): IO[Int] =
      for {
        uri         <- validateUri(s"$hostUrl/resource")
        accessToken <- send(HttpRequest(request(GET, uri), requestName))(mapResponse)
      } yield accessToken

    private lazy val mapResponse: PartialFunction[(Status, Request[IO], Response[IO]), IO[Int]] = {
      case (Status.Ok, _, response) => response.as[String].map(_.toInt)
    }
  }
}
