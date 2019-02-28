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

package ch.datascience.webhookservice.project

import cats.MonadError
import cats.implicits._
import ch.datascience.generators.Generators.Implicits._
import ch.datascience.webhookservice.generators.ServiceTypesGenerators._
import org.scalatest.Matchers._
import org.scalatest.WordSpec
import play.api.Configuration

import scala.util.{Failure, Success, Try}

class SelfUrlConfigProviderSpec extends WordSpec {

  private implicit val context: MonadError[Try, Throwable] = MonadError[Try, Throwable]

  "get" should {

    "return SelfUrl" in {
      val selfUrl = selfUrls.generateOne
      val config = Configuration.from(
        Map(
          "services" -> Map(
            "self" -> Map(
              "url" -> selfUrl.toString()
            )
          )
        )
      )

      new SelfUrlConfig[Try](config).get() shouldBe Success(selfUrl)
    }

    "fail if the value for 'services.self.url' is invalid" in {
      val config = Configuration.from(
        Map(
          "services" -> Map(
            "self" -> Map(
              "url" -> "123"
            )
          )
        )
      )

      val Failure(exception) = new SelfUrlConfig[Try](config).get()

      exception.getMessage should include(
        "'123' is not a valid ch.datascience.webhookservice.project.SelfUrlConfig.SelfUrl"
      )
    }

    "fail if there is no 'services.self.url' in the config" in {
      val config = Configuration.empty

      new SelfUrlConfig[Try](config).get() shouldBe a[Failure[_]]
    }
  }
}