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

package ch.datascience.knowledgegraph.datasets.rest

import cats.effect.IO
import cats.implicits._
import ch.datascience.generators.Generators.Implicits._
import ch.datascience.graph.model.GraphModelGenerators._
import ch.datascience.graph.model.datasets.SameAs
import ch.datascience.interpreters.TestLogger
import ch.datascience.knowledgegraph.datasets.DatasetsGenerators._
import ch.datascience.logging.TestExecutionTimeRecorder
import ch.datascience.rdfstore.entities.DataSet
import ch.datascience.rdfstore.entities.bundles._
import ch.datascience.rdfstore.{InMemoryRdfStore, SparqlQueryTimeRecorder}
import ch.datascience.stubbing.ExternalServiceStubbing
import io.renku.jsonld.EntityId
import org.scalatest.Matchers._
import org.scalatest.WordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class IOProjectDatasetsFinderSpec
    extends WordSpec
    with InMemoryRdfStore
    with ExternalServiceStubbing
    with ScalaCheckPropertyChecks {

  "findProjectDatasets" should {

    "return all datasets of the given project" in new TestCase {
      forAll(projectPaths, datasets, datasets) { (projectPath, dataset1, dataset2) =>
        loadToStore(
          randomDataSetCommit,
          dataSetCommit()(projectPath)(
            datasetIdentifier  = dataset1.id,
            datasetName        = dataset1.name,
            maybeDatasetSameAs = dataset1.sameAs.some
          ),
          dataSetCommit()(projectPath)(
            datasetIdentifier  = dataset2.id,
            datasetName        = dataset2.name,
            maybeDatasetSameAs = dataset2.sameAs.some
          )
        )

        datasetsFinder.findProjectDatasets(projectPath).unsafeRunSync() should contain theSameElementsAs List(
          (dataset1.id, dataset1.name, dataset1.sameAs),
          (dataset2.id, dataset2.name, dataset2.sameAs)
        )
      }
    }

    "return datasets of the given project with sameAs from the very top ancestor " +
      "- case with an external dataset" in new TestCase {
      forAll(projectPaths, datasets, projectPaths, datasets) { (project1, dataset1, project2, dataset2) =>
        loadToStore(
          dataSetCommit()(project1)(
            datasetIdentifier  = dataset1.id,
            datasetName        = dataset1.name,
            maybeDatasetSameAs = dataset1.sameAs.some
          ),
          dataSetCommit()(project2)(
            datasetIdentifier  = dataset2.id,
            datasetName        = dataset2.name,
            maybeDatasetSameAs = DataSet.entityId(dataset1.id).asSameAs.some
          )
        )

        datasetsFinder.findProjectDatasets(project1).unsafeRunSync() should contain theSameElementsAs List(
          (dataset1.id, dataset1.name, dataset1.sameAs)
        )
        datasetsFinder.findProjectDatasets(project2).unsafeRunSync() should contain theSameElementsAs List(
          (dataset2.id, dataset2.name, dataset1.sameAs)
        )
      }
    }

    "return datasets of the given project with sameAs from the very top ancestor " +
      "- case with in-renku created dataset" in new TestCase {
      forAll(projectPaths, datasets, projectPaths, datasets) { (project1, dataset1, project2, dataset2) =>
        val dataSet1BasedSameAs = DataSet.entityId(dataset1.id).asSameAs

        loadToStore(
          dataSetCommit()(project1)(
            datasetIdentifier  = dataset1.id,
            datasetName        = dataset1.name,
            maybeDatasetSameAs = None
          ),
          dataSetCommit()(project2)(
            datasetIdentifier  = dataset2.id,
            datasetName        = dataset2.name,
            maybeDatasetSameAs = dataSet1BasedSameAs.some
          )
        )

        datasetsFinder.findProjectDatasets(project1).unsafeRunSync() should contain theSameElementsAs List(
          (dataset1.id, dataset1.name, dataSet1BasedSameAs)
        )
        datasetsFinder.findProjectDatasets(project2).unsafeRunSync() should contain theSameElementsAs List(
          (dataset2.id, dataset2.name, dataSet1BasedSameAs)
        )
      }
    }

    "return None if there are no datasets in the project" in new TestCase {
      val projectPath = projectPaths.generateOne
      datasetsFinder.findProjectDatasets(projectPath).unsafeRunSync() shouldBe List.empty
    }
  }

  private implicit class EntityIdOps(entityId: EntityId) {
    lazy val asSameAs: SameAs = SameAs.fromId(entityId.value).fold(throw _, identity)
  }

  private trait TestCase {
    private val logger       = TestLogger[IO]()
    private val timeRecorder = new SparqlQueryTimeRecorder(TestExecutionTimeRecorder(logger))
    val datasetsFinder       = new IOProjectDatasetsFinder(rdfStoreConfig, renkuBaseUrl, logger, timeRecorder)
  }
}
