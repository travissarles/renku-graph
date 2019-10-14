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

package ch.datascience.graph.acceptancetests.knowledgegraph

import ch.datascience.generators.Generators.Implicits._
import ch.datascience.graph.acceptancetests.data._
import ch.datascience.graph.acceptancetests.flows.RdfStoreProvisioning.`data in the RDF store`
import ch.datascience.graph.acceptancetests.testing.AcceptanceTestPatience
import ch.datascience.graph.acceptancetests.tooling.GraphServices
import ch.datascience.graph.acceptancetests.tooling.ResponseTools._
import ch.datascience.graph.acceptancetests.tooling.TestReadabilityTools._
import ch.datascience.graph.model.EventsGenerators._
import ch.datascience.graph.model.GraphModelGenerators._
import ch.datascience.graph.model.datasets.Identifier
import ch.datascience.graph.model.events.CommittedDate
import ch.datascience.http.client.UrlEncoder.urlEncode
import ch.datascience.http.rest.Links.{Href, Link, Rel, _links}
import ch.datascience.http.server.EndpointTester._
import ch.datascience.knowledgegraph.datasets.DatasetsGenerators._
import ch.datascience.knowledgegraph.datasets.model._
import ch.datascience.rdfstore.triples.{singleFileAndCommitWithDataset, triples}
import ch.datascience.tinytypes.json.TinyTypeDecoders._
import io.circe.Json
import io.circe.literal._
import org.http4s.Status._
import org.scalatest.Matchers._
import org.scalatest.{FeatureSpec, GivenWhenThen}

import scala.util.Random

class DatasetsResourcesSpec extends FeatureSpec with GivenWhenThen with GraphServices with AcceptanceTestPatience {

  private val project          = projects.generateOne
  private val dataset1CommitId = commitIds.generateOne
  private val dataset1Creation = datasetInProjectCreations.generateOne
  private val dataset1 = datasets.generateOne.copy(
    maybeDescription = Some(datasetDescriptions.generateOne),
    published        = datasetPublishingInfos.generateOne.copy(maybeDate = Some(datasetPublishedDates.generateOne)),
    project          = List(DatasetProject(project.path, dataset1Creation))
  )
  private val dataset2Creation = datasetInProjectCreations.generateOne
  private val dataset2CommitId = commitIds.generateOne
  private val dataset2 = datasets.generateOne.copy(
    maybeDescription = None,
    published        = datasetPublishingInfos.generateOne.copy(maybeDate = None),
    project          = List(DatasetProject(project.path, dataset2Creation))
  )

  feature("GET knowledge-graph/projects/<namespace>/<name>/datasets to find project's datasets") {

    scenario("As a user I would like to find project's datasets by calling a REST enpoint") {

      Given("some data in the RDF Store")
      val jsonLDTriples = triples(
        singleFileAndCommitWithDataset(
          project.path,
          dataset1CommitId,
          dataset1Creation.agent.name,
          dataset1Creation.agent.email,
          dataset1Creation.date.toUnsafe(date => CommittedDate.from(date.value)),
          dataset1.id,
          dataset1.name,
          dataset1.maybeDescription,
          dataset1.published.maybeDate,
          dataset1.published.creators.map(creator => (creator.name, creator.maybeEmail)),
          dataset1.part.map(part => (part.name, part.atLocation)),
          schemaVersion = currentSchemaVersion
        ),
        singleFileAndCommitWithDataset(
          project.path,
          dataset2CommitId,
          dataset2Creation.agent.name,
          dataset2Creation.agent.email,
          dataset2Creation.date.toUnsafe(date => CommittedDate.from(date.value)),
          dataset2.id,
          dataset2.name,
          dataset2.maybeDescription,
          dataset2.published.maybeDate,
          dataset2.published.creators.map(creator => (creator.name, creator.maybeEmail)),
          dataset2.part.map(part => (part.name, part.atLocation)),
          schemaVersion = currentSchemaVersion
        )
      )

      `data in the RDF store`(project, dataset1CommitId, jsonLDTriples)

      When("user fetches project's datasets with GET knowledge-graph/projects/<project-name>/datasets")
      val projectDatasetsResponse = knowledgeGraphClient GET s"knowledge-graph/projects/${project.path}/datasets"

      Then("he should get OK response with project's datasets")
      projectDatasetsResponse.status shouldBe Ok
      val Right(foundDatasets) = projectDatasetsResponse.bodyAsJson.as[List[Json]]
      foundDatasets should contain theSameElementsAs List(shortJson(dataset1), shortJson(dataset2))

      When("user then fetches details of the chosen dataset with the link from the response")
      val someDatasetDetailsLink =
        Random
          .shuffle(foundDatasets)
          .headOption
          .flatMap(_._links.get(Rel("details")))
          .getOrFail(message = "No link with rel 'details'")
      val datasetDetailsResponse = restClient GET someDatasetDetailsLink.toString

      Then("he should get OK response with dataset details")
      datasetDetailsResponse.status shouldBe Ok
      val Right(foundDatasetDetails) = datasetDetailsResponse.bodyAsJson.as[Json]
      val expectedDataset = List(dataset1, dataset2)
        .find(dataset => someDatasetDetailsLink.value contains urlEncode(dataset.id.value))
        .getOrFail(message = "Returned 'details' link does not point to any dataset in the RDF store")

      foundDatasetDetails.hcursor.downField("identifier").as[Identifier] shouldBe Right(expectedDataset.id)
    }
  }

  private def shortJson(dataset: Dataset) =
    json"""
    {
      "identifier": ${dataset.id.value}, 
      "name": ${dataset.name.value}
    }""".deepMerge {
      _links(
        Link(Rel("details"), Href(renkuResourceUrl / "datasets" / dataset.id.value))
      )
    }
}