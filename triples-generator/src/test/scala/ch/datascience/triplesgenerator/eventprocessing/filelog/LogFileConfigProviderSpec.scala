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

package ch.datascience.triplesgenerator.eventprocessing.filelog

import java.nio.file.Paths

import cats.implicits._
import ch.datascience.config.ConfigLoader.ConfigLoadingException
import ch.datascience.generators.Generators.Implicits._
import ch.datascience.generators.Generators.relativePaths
import com.typesafe.config.ConfigFactory
import org.scalatest.Matchers._
import org.scalatest.WordSpec

import scala.collection.JavaConverters._
import scala.util.{Failure, Success, Try}

class LogFileConfigProviderSpec extends WordSpec {

  "get" should {

    "read 'file-event-log.file-path' from the config" in {
      val pathAsString = relativePaths().generateOne
      val config = ConfigFactory.parseMap(
        Map(
          "file-event-log" -> Map(
            "file-path" -> pathAsString
          ).asJava
        ).asJava
      )

      new LogFileConfigProvider[Try](config).get shouldBe Success(Paths.get(pathAsString))
    }

    "fail if there's no 'file-event-log.file-path' entry" in {
      val config = ConfigFactory.empty()

      val Failure(exception) = new LogFileConfigProvider[Try](config).get

      exception shouldBe an[ConfigLoadingException]
    }
  }
}