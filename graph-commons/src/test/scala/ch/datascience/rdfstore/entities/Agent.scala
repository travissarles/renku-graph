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

package ch.datascience.rdfstore.entities

import ch.datascience.graph.model.SchemaVersion

final case class Agent(schemaVersion: SchemaVersion, maybeStartedBy: Option[Person] = None)

object Agent {

  import io.renku.jsonld._
  import io.renku.jsonld.syntax._

  implicit lazy val encoder: JsonLDEncoder[Agent] = JsonLDEncoder.instance { entity =>
    JsonLD.entity(
      EntityId of s"https://github.com/swissdatasciencecenter/renku-python/tree/v${entity.schemaVersion}",
      EntityTypes of (prov / "SoftwareAgent", wfprov / "WorkflowEngine"),
      rdfs / "label"        -> s"renku ${entity.schemaVersion}".asJsonLD,
      prov / "wasStartedBy" -> entity.maybeStartedBy.asJsonLD
    )
  }
}
