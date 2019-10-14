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

package ch.datascience.rdfstore.triples
package entities

import ch.datascience.graph.model.events.CommitId
import ch.datascience.rdfstore.FusekiBaseUrl
import ch.datascience.rdfstore.triples.entities.Project.`schema:isPartOf`
import io.circe.Json
import io.circe.literal._

private[triples] object AssociationActivity {

  // format: off
  def apply(id:              Id,
            projectId:       Project.Id,
            comment:         String,
            qualifiedUsages: List[UsageEntity.Id],
            maybeAgentId:    Option[Agent.Id]): Json = json"""
  {
    "@id": $id,
    "@type": [
      "prov:Activity",
      "http://purl.org/wf4ever/wfprov#ProcessRun"
    ],
    "prov:qualifiedAssociation": {
        "@id": ${s"${id.commitId}/association"}
    },
    "rdfs:comment": $comment
  }""".deepMerge(`schema:isPartOf`(projectId))
      .deepMerge(qualifiedUsages toResources "prov:qualifiedUsage")
      .deepMerge(maybeAgentId toResource "prov:agent")
  // format: on

  final case class Id(commitId: CommitId)(implicit fusekiBaseUrl: FusekiBaseUrl) extends EntityId {
    override val value: String = (fusekiBaseUrl / "commit" / commitId).toString
  }
}