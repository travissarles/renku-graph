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
import ch.datascience.generators.Generators.Implicits._
import ch.datascience.graph.model.GraphModelGenerators._
import ch.datascience.interpreters.TestLogger
import ch.datascience.logging.TestExecutionTimeRecorder
import ch.datascience.rdfstore.InMemoryRdfStore
import ch.datascience.rdfstore.triples._
import org.scalatest.Matchers._
import org.scalatest.WordSpec

class IOTriplesRemoverSpec extends WordSpec with InMemoryRdfStore {

  "removeAllTriples" should {

    "remove all the triples from the storage" in new TestCase {

      loadToStore(
        triples(
          singleFileAndCommitWithDataset(projectPaths.generateOne),
          singleFileAndCommitWithDataset(projectPaths.generateOne)
        )
      )

      rdfStoreSize should be > 0

      triplesRemover
        .removeAllTriples()
        .unsafeRunSync() shouldBe ((): Unit)

      rdfStoreSize shouldBe 0
    }
  }

  private trait TestCase {
    val logger                = TestLogger[IO]()
    val executionTimeRecorder = TestExecutionTimeRecorder[IO](logger)
    val triplesRemover        = new IOTriplesRemover(rdfStoreConfig, executionTimeRecorder, logger)
  }
}