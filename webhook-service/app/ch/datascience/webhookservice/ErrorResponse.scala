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

package ch.datascience.webhookservice

import ch.datascience.tinytypes.constraints.NonBlank
import ch.datascience.tinytypes.{ TinyType, TinyTypeFactory }
import play.api.libs.json.{ JsError, JsValue, Json, Writes }

class ErrorResponse private ( val value: String ) extends AnyVal with TinyType[String]
object ErrorResponse extends TinyTypeFactory[String, ErrorResponse]( new ErrorResponse( _ ) ) with NonBlank {

  implicit val errorResponseWrites: Writes[ErrorResponse] = Writes[ErrorResponse] {
    case ErrorResponse( message ) => Json.obj( "error" -> message )
  }

  def apply( jsError: JsError ): ErrorResponse = {
    val errorMessage = jsError.errors.foldLeft( "Json deserialization error(s):" ) {
      case ( message, ( path, pathErrors ) ) =>
        s"$message\n $path -> ${pathErrors.map( _.message ).mkString( "; " )}"
    }
    ErrorResponse( errorMessage )
  }

  implicit class ErrorResponseOps( errorResponse: ErrorResponse ) {
    lazy val toJson: JsValue = Json.toJson( errorResponse )
  }
}
