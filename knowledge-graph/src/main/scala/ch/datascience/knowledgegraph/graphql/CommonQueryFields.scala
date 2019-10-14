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

package ch.datascience.knowledgegraph.graphql

import ch.datascience.graph.model.projects.ProjectPath
import ch.datascience.knowledgegraph.graphql.Arguments._
import eu.timepit.refined.auto._
import sangria.schema._

import scala.language.higherKinds

object CommonQueryFields {

  val projectPathArgument = Argument(
    name = "projectPath",
    argumentType = ProjectPath.toScalarType(
      description      = "Project's path in the GitLab.",
      exceptionMessage = "ProjectPath value expected in format <namespace>/<project>"
    )
  )
}