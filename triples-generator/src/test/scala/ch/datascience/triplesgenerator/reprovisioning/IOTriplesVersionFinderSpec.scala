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

package ch.datascience.triplesgenerator.reprovisioning

import cats.effect.IO
import ch.datascience.generators.CommonGraphGenerators._
import ch.datascience.generators.Generators.Implicits._
import ch.datascience.graph.model.EventsGenerators._
import ch.datascience.graph.model.GraphModelGenerators._
import ch.datascience.graph.model.SchemaVersion
import ch.datascience.graph.model.events.CommitId
import ch.datascience.interpreters.TestLogger
import ch.datascience.interpreters.TestLogger.Level.Warn
import ch.datascience.logging.TestExecutionTimeRecorder
import ch.datascience.rdfstore.entities.Person.persons
import ch.datascience.rdfstore.entities.bundles._
import ch.datascience.rdfstore.entities.{Activity, Agent, Project}
import ch.datascience.rdfstore.{InMemoryRdfStore, SparqlQueryTimeRecorder}
import io.renku.jsonld.syntax._
import org.scalatest.Matchers._
import org.scalatest.WordSpec

class IOTriplesVersionFinderSpec extends WordSpec with InMemoryRdfStore {

  "triplesUpToDate" should {

    "return true if there's a single commit Activity SoftwareAgent entity with the current version of Renku" in new TestCase {

      loadToStore(commitActivity(schemaVersion).asJsonLD)

      triplesVersionFinder.triplesUpToDate.unsafeRunSync() shouldBe true

      logger.loggedOnly(Warn(s"renku version find finished${executionTimeRecorder.executionTimeInfo}"))
    }

    "return false if there's a single SoftwareAgent entity with some old version of Renku" in new TestCase {

      loadToStore(commitActivity(schemaVersions generateDifferentThan schemaVersion).asJsonLD)

      triplesVersionFinder.triplesUpToDate.unsafeRunSync() shouldBe false

      logger.loggedOnly(Warn(s"renku version find finished${executionTimeRecorder.executionTimeInfo}"))
    }

    "return false if there are multiple SoftwareAgent entities with different versions of Renku" in new TestCase {

      loadToStore(commitActivity(schemaVersion).asJsonLD,
                  commitActivity(schemaVersions generateDifferentThan schemaVersion).asJsonLD)

      triplesVersionFinder.triplesUpToDate.unsafeRunSync() shouldBe false

      logger.loggedOnly(Warn(s"renku version find finished${executionTimeRecorder.executionTimeInfo}"))
    }

    "return false if SoftwareAgent points to the current versions of Renku but it's not linked to a commit activity" in new TestCase {

      loadToStore(Agent(schemaVersion).asJsonLD)

      triplesVersionFinder.triplesUpToDate.unsafeRunSync() shouldBe false

      logger.loggedOnly(Warn(s"renku version find finished${executionTimeRecorder.executionTimeInfo}"))
    }
  }

  private trait TestCase {
    val schemaVersion              = schemaVersions.generateOne
    val logger                     = TestLogger[IO]()
    val executionTimeRecorder      = TestExecutionTimeRecorder(logger)
    private val sparqlTimeRecorder = new SparqlQueryTimeRecorder(executionTimeRecorder)
    val triplesVersionFinder       = new IOTriplesVersionFinder(rdfStoreConfig, schemaVersion, logger, sparqlTimeRecorder)
  }

  private def commitActivity(schemaVersion: SchemaVersion, commitId: CommitId = commitIds.generateOne) =
    Activity(
      commitIds.generateOne,
      committedDates.generateOne,
      persons.generateOne,
      Project(projectPaths.generateOne, projectNames.generateOne, projectCreatedDates.generateOne, maybeCreator = None),
      Agent(schemaVersion)
    ).asJsonLD
}
