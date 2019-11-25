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

package ch.datascience.triplesgenerator.reprovisioning

import cats.effect.IO
import ch.datascience.generators.CommonGraphGenerators._
import ch.datascience.generators.Generators.Implicits._
import ch.datascience.interpreters.TestLogger
import ch.datascience.interpreters.TestLogger.Level.Warn
import ch.datascience.logging.TestExecutionTimeRecorder
import ch.datascience.rdfstore.InMemoryRdfStore
import ch.datascience.rdfstore.triples._
import ch.datascience.rdfstore.triples.entities._
import org.scalatest.Matchers._
import org.scalatest.WordSpec

class IOTriplesVersionFinderSpec extends WordSpec with InMemoryRdfStore {

  "triplesUpToDate" should {

    "return true if there's a single SoftwareAgent entity with the current version of Renku" in new TestCase {

      loadToStore(
        triples(
          List(
            Agent(Agent.Id(schemaVersion))
          )
        )
      )

      triplesVersionFinder.triplesUpToDate.unsafeRunSync() shouldBe true

      logger.loggedOnly(Warn(s"Checking if triples are up to date done${executionTimeRecorder.executionTimeInfo}"))
    }

    "return false if there's a single SoftwareAgent entity with some old version of Renku" in new TestCase {

      loadToStore(
        triples(
          List(
            Agent(Agent.Id(schemaVersions generateDifferentThan schemaVersion))
          )
        )
      )

      triplesVersionFinder.triplesUpToDate.unsafeRunSync() shouldBe false

      logger.loggedOnly(Warn(s"Checking if triples are up to date done${executionTimeRecorder.executionTimeInfo}"))
    }

    "return false if there are multiple SoftwareAgent entities event if there's one with the current version of Renku" in new TestCase {

      loadToStore(
        triples(
          List(
            Agent(Agent.Id(schemaVersions generateDifferentThan schemaVersion)),
            Agent(Agent.Id(schemaVersion))
          )
        )
      )

      triplesVersionFinder.triplesUpToDate.unsafeRunSync() shouldBe false

      logger.loggedOnly(Warn(s"Checking if triples are up to date done${executionTimeRecorder.executionTimeInfo}"))
    }
  }

  private trait TestCase {
    val schemaVersion         = schemaVersions.generateOne
    val logger                = TestLogger[IO]()
    val executionTimeRecorder = TestExecutionTimeRecorder[IO](logger)
    val triplesVersionFinder  = new IOTriplesVersionFinder(rdfStoreConfig, executionTimeRecorder, schemaVersion, logger)
  }
}