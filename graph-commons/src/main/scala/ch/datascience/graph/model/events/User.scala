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

package ch.datascience.graph.model.events

import ch.datascience.tinytypes.constraints.NonBlank
import ch.datascience.tinytypes.{StringTinyType, TinyTypeFactory}
import io.circe.Decoder

case class User(
    username: Username,
    email:    Email
)

class Username private (val value: String) extends AnyVal with StringTinyType
object Username extends TinyTypeFactory[Username](new Username(_)) with NonBlank {
  implicit lazy val userNameDecoder: Decoder[Username] = Decoder.decodeString.map(Username.apply)
}

class Email private (val value: String) extends AnyVal with StringTinyType
object Email extends TinyTypeFactory[Email](new Email(_)) with NonBlank {
  implicit lazy val emailDecoder: Decoder[Email] = Decoder.decodeString.map(Email.apply)
}