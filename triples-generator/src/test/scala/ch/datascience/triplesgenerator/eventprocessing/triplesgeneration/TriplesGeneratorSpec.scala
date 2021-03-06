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

package ch.datascience.triplesgenerator.eventprocessing.triplesgeneration

import cats.effect.{ContextShift, IO, Timer}
import ch.datascience.triplesgenerator.config.TriplesGeneration
import ch.datascience.triplesgenerator.config.TriplesGeneration._
import ch.datascience.triplesgenerator.eventprocessing.triplesgeneration.renkulog.RenkuLogTriplesGenerator
import com.typesafe.config.ConfigFactory
import org.scalatest.Matchers._
import org.scalatest.WordSpec

import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext

class TriplesGeneratorSpec extends WordSpec {

  "apply" should {

    s"return an instance of RenkuLogTriplesGenerator if TriplesGeneration is $RenkuLog" in {
      TriplesGenerator(TriplesGeneration.RenkuLog).unsafeRunSync() shouldBe a[RenkuLogTriplesGenerator]
    }

    s"return an instance of RemoteTriplesGenerator if TriplesGeneration is $RemoteTriplesGeneration" in {

      val config = ConfigFactory.parseMap(
        Map(
          "services" -> Map("triples-generator" -> Map("url" -> "http://host").asJava).asJava
        ).asJava
      )

      TriplesGenerator(TriplesGeneration.RemoteTriplesGeneration, config)
        .unsafeRunSync() shouldBe a[RemoteTriplesGenerator]
    }
  }

  private implicit val ec:    ExecutionContext = ExecutionContext.global
  private implicit val cs:    ContextShift[IO] = IO.contextShift(ExecutionContext.global)
  private implicit val timer: Timer[IO]        = IO.timer(ExecutionContext.global)
}
