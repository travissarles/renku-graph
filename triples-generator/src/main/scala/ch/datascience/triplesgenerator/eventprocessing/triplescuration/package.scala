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

package ch.datascience.triplesgenerator.eventprocessing

import ch.datascience.rdfstore.SparqlValueEncoder.sparqlEncode
import ch.datascience.tinytypes.TinyType

package object triplescuration {

  def `INSERT DATA`[TT <: TinyType { type V = String }](resource: String, property: String, value: TT): String =
    s"INSERT DATA { $resource $property '${sparqlEncode(value.value)}'}"
}
