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
import cats.implicits._
import ch.datascience.generators.Generators.Implicits._
import ch.datascience.triplesgenerator.eventprocessing.triplescuration.CurationGenerators._
import ch.datascience.triplesgenerator.eventprocessing.triplesuploading.TriplesUploadResult._
import org.scalamock.scalatest.MockFactory
import org.scalatest.Matchers._
import org.scalatest.WordSpec

import scala.util.Try

class UploaderSpec extends WordSpec with MockFactory {

  "upload" should {

    s"return $DeliverySuccess if triples and updates uploading is successful" in new TestCase {

      inSequence {
        (triplesUploader.upload _)
          .expects(curatedTriples.triples)
          .returning(context.pure(DeliverySuccess))

        (updatesUploader.send _)
          .expects(curatedTriples.updates)
          .returning(context.pure(DeliverySuccess))
      }

      uploader.upload(curatedTriples) shouldBe context.pure(DeliverySuccess)
    }

    s"return $DeliveryFailure if triples uploading failed with such failure" in new TestCase {

      val failure = DeliveryFailure("error")
      (triplesUploader.upload _)
        .expects(curatedTriples.triples)
        .returning(context.pure(failure))

      uploader.upload(curatedTriples) shouldBe context.pure(failure)
    }

    s"return $DeliveryFailure if updates uploading failed with such failure" in new TestCase {

      (triplesUploader.upload _)
        .expects(curatedTriples.triples)
        .returning(context.pure(DeliverySuccess))

      val failure = DeliveryFailure("error")
      (updatesUploader.send _)
        .expects(curatedTriples.updates)
        .returning(context.pure(failure))

      uploader.upload(curatedTriples) shouldBe context.pure(failure)
    }

    s"return $InvalidTriplesFailure if triples uploading failed with such failure" in new TestCase {

      val failure = InvalidTriplesFailure("error")
      (triplesUploader.upload _)
        .expects(curatedTriples.triples)
        .returning(context.pure(failure))

      uploader.upload(curatedTriples) shouldBe context.pure(failure)
    }

    s"return $InvalidUpdatesFailure if updates uploading failed with such failure" in new TestCase {

      (triplesUploader.upload _)
        .expects(curatedTriples.triples)
        .returning(context.pure(DeliverySuccess))

      val failure = InvalidUpdatesFailure("error")
      (updatesUploader.send _)
        .expects(curatedTriples.updates)
        .returning(context.pure(failure))

      uploader.upload(curatedTriples) shouldBe context.pure(failure)
    }
  }

  private trait TestCase {
    val context = MonadError[Try, Throwable]

    val curatedTriples = curatedTriplesObjects.generateOne

    val triplesUploader = mock[TriplesUploader[Try]]
    val updatesUploader = mock[UpdatesUploader[Try]]
    val uploader        = new Uploader[Try](triplesUploader, updatesUploader)
  }
}
