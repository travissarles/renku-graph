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

package ch.datascience.graph.model.views

import ch.datascience.tinytypes.{Renderer, TinyType}

/*
 * This is a marker trait to be used with TinyTypes so they can be rendered as an RdfResource which is `<url>`
 */
trait RdfResource

object RdfResource {
  implicit object RdfResourceRenderer extends Renderer[RdfResource, TinyType] {
    override def render(value: TinyType): String = s"<$value>"
  }
}