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

package ch.datascience.knowledgegraph.datasets

import ch.datascience.graph.model.datasets._
import ch.datascience.graph.model.projects
import ch.datascience.graph.model.projects.Path
import ch.datascience.graph.model.users.{Affiliation, Email, Name => UserName}

object model {

  final case class Dataset(id:               Identifier,
                           name:             Name,
                           sameAs:           SameAs,
                           maybeUrl:         Option[Url],
                           maybeDescription: Option[Description],
                           published:        DatasetPublishing,
                           parts:            List[DatasetPart],
                           projects:         List[DatasetProject])

  final case class DatasetPublishing(maybeDate: Option[PublishedDate], creators: Set[DatasetCreator])
  final case class DatasetCreator(maybeEmail:   Option[Email], name:             UserName, maybeAffiliation: Option[Affiliation])

  final case class DatasetPart(name: PartName, atLocation: PartLocation)

  final case class DatasetProject(path:     Path, name:                  projects.Name, created: AddedToProject)
  final case class AddedToProject(date:     DateCreatedInProject, agent: DatasetAgent)
  final case class DatasetAgent(maybeEmail: Option[Email], name:         UserName)
}
