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

package ch.datascience.triplesgenerator.eventprocessing.triplescuration.forks

import ch.datascience.rdfstore.JsonLDTriples
import io.circe.Json
import io.circe.optics.JsonOptics._
import io.circe.optics.JsonPath._
import monocle.function.Plated

private class ProjectPropertiesRemover extends (JsonLDTriples => JsonLDTriples) {

  override def apply(triples: JsonLDTriples): JsonLDTriples = JsonLDTriples(
    Plated.transform(toJsonWithoutCreatorDetails)(triples.value)
  )

  private def toJsonWithoutCreatorDetails(json: Json): Json =
    root.`@type`.each.string.getAll(json) match {
      case types if types.contains("http://schema.org/Project") =>
        json remove "http://schema.org/creator" remove "http://schema.org/dateCreated"
      case _ => json
    }

  private implicit class JsonOps(json: Json) {
    def remove(property: String): Json = root.obj.modify(_.remove(property))(json)
  }
}
