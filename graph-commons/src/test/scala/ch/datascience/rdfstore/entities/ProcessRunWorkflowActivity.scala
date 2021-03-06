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

import java.time.Instant
import java.time.temporal.ChronoUnit.SECONDS

import ch.datascience.graph.model.events.{CommitId, CommittedDate}

final case class ProcessRunWorkflowActivity(override val id:            CommitId,
                                            override val committedDate: CommittedDate,
                                            override val committer:     Person,
                                            override val project:       Project,
                                            override val agent:         Agent,
                                            override val comment:       String,
                                            cwlFile:                    CwlFile,
                                            informedBy:                 Activity,
                                            association:                Association,
                                            startTime:                  Instant = Instant.now(),
                                            endTime:                    Instant = Instant.now().plus(10, SECONDS),
                                            influenced:                 List[String] = Nil,
                                            usages:                     List[Usage])
    extends Activity(id, committedDate, committer, project, agent, comment, Some(informedBy))

object ProcessRunWorkflowActivity {

  import ch.datascience.graph.config.RenkuBaseUrl
  import ch.datascience.rdfstore.FusekiBaseUrl
  import io.renku.jsonld._
  import io.renku.jsonld.syntax._

  implicit def encoder(implicit renkuBaseUrl: RenkuBaseUrl,
                       fusekiBaseUrl:         FusekiBaseUrl): JsonLDEncoder[ProcessRunWorkflowActivity] =
    JsonLDEncoder.instance { entity =>
      JsonLD.entity(
        EntityId of fusekiBaseUrl / "activities" / "commit" / entity.id,
        EntityTypes of (wfprov / "ProcessRun", wfprov / "WorkflowRun", prov / "Activity"),
        Activity.toProperties(entity),
        rdfs / "label"                -> s"${entity.cwlFile}@${entity.id}".asJsonLD,
        prov / "qualifiedAssociation" -> entity.association.asJsonLD,
        prov / "influenced"           -> entity.influenced.asJsonLD,
        prov / "atLocation"           -> entity.association.processPlan.cwlFile.asJsonLD,
        prov / "qualifiedUsage"       -> entity.usages.asJsonLD
      )
    }
}
