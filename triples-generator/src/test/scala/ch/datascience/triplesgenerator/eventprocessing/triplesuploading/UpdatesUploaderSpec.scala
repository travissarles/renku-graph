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

import cats.effect.{ContextShift, IO, Timer}
import ch.datascience.generators.CommonGraphGenerators.rdfStoreConfigs
import ch.datascience.generators.Generators.Implicits._
import ch.datascience.generators.Generators._
import ch.datascience.http.client.UrlEncoder.urlEncode
import ch.datascience.interpreters.TestLogger
import ch.datascience.logging.TestExecutionTimeRecorder
import ch.datascience.rdfstore.{FusekiBaseUrl, SparqlQueryTimeRecorder}
import ch.datascience.stubbing.ExternalServiceStubbing
import ch.datascience.triplesgenerator.eventprocessing.triplescuration.CuratedTriples.Update
import ch.datascience.triplesgenerator.eventprocessing.triplescuration.CurationGenerators._
import ch.datascience.triplesgenerator.eventprocessing.triplesuploading.TriplesUploadResult.{DeliveryFailure, DeliverySuccess, InvalidUpdatesFailure}
import com.github.tomakehurst.wiremock.client.ResponseDefinitionBuilder
import com.github.tomakehurst.wiremock.client.WireMock._
import eu.timepit.refined.auto._
import org.http4s.Status
import org.scalatest.Matchers._
import org.scalatest.WordSpec

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.language.postfixOps

class UpdatesUploaderSpec extends WordSpec with ExternalServiceStubbing {

  "send" should {

    s"return $DeliverySuccess if all the given updates pass" in new TestCase {
      val updates = nonEmptyList(curationUpdates).generateOne.toList

      updates foreach { update =>
        givenStore(forUpdate = update, returning = ok())
      }

      updater.send(updates).unsafeRunSync() shouldBe DeliverySuccess
    }

    s"return $InvalidUpdatesFailure if one or more of the given updates is invalid (RDF store responds with BAD_REQUEST 400)" in new TestCase {

      val update1 = curationUpdates.generateOne
      givenStore(forUpdate = update1, returning = ok())

      val update2      = curationUpdates.generateOne
      val errorMessage = nonEmptyStrings().generateOne
      givenStore(forUpdate = update2, returning = badRequest().withBody(errorMessage))

      val updates = List(update1, update2)

      updater.send(updates).unsafeRunSync() shouldBe InvalidUpdatesFailure(
        s"Triples curation update '${update2.name}' failed: $errorMessage"
      )
    }

    s"return $DeliveryFailure if remote responds with status different than OK or BAD_REQUEST for at least one update" in new TestCase {

      val update1 = curationUpdates.generateOne
      givenStore(forUpdate = update1, returning = ok())

      val update2      = curationUpdates.generateOne
      val errorMessage = nonEmptyStrings().generateOne
      givenStore(forUpdate = update2, returning = serviceUnavailable().withBody(errorMessage))

      val updates = List(update1, update2)

      updater.send(updates).unsafeRunSync() shouldBe DeliveryFailure(
        s"Triples curation update failed: ${Status.ServiceUnavailable}: $errorMessage"
      )
    }

    s"return $DeliveryFailure for connectivity issues" in new TestCase {

      val fusekiBaseUrl = localHttpUrls.map(FusekiBaseUrl.apply).generateOne
      override val rdfStoreConfig = rdfStoreConfigs.generateOne.copy(
        fusekiBaseUrl = fusekiBaseUrl
      )

      updater.send(List(curationUpdates.generateOne)).unsafeRunSync() shouldBe DeliveryFailure(
        s"POST $fusekiBaseUrl/${rdfStoreConfig.datasetName}/update error: Connection refused"
      )
    }
  }

  private implicit val cs:    ContextShift[IO] = IO.contextShift(global)
  private implicit val timer: Timer[IO]        = IO.timer(global)

  private trait TestCase {
    val logger               = TestLogger[IO]()
    private val timeRecorder = new SparqlQueryTimeRecorder(TestExecutionTimeRecorder(logger))
    val rdfStoreConfig = rdfStoreConfigs.generateOne.copy(
      fusekiBaseUrl = FusekiBaseUrl(externalServiceBaseUrl)
    )
    lazy val updater = new IOUpdatesUploader(
      rdfStoreConfig,
      logger,
      timeRecorder,
      retryInterval = 100 millis,
      maxRetries    = 1
    )

    def givenStore(forUpdate: Update, returning: ResponseDefinitionBuilder) =
      stubFor {
        post(s"/${rdfStoreConfig.datasetName}/update")
          .withBasicAuth(rdfStoreConfig.authCredentials.username.value, rdfStoreConfig.authCredentials.password.value)
          .withHeader("content-type", equalTo("application/x-www-form-urlencoded"))
          .withRequestBody(equalTo(s"update=${urlEncode(forUpdate.query.toString)}"))
          .willReturn(returning)
      }
  }
}
