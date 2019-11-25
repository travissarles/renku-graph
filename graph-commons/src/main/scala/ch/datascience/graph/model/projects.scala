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

package ch.datascience.graph.model

import java.time.Instant

import ch.datascience.graph.config.RenkuBaseUrl
import ch.datascience.tinytypes._
import ch.datascience.tinytypes.constraints._

object projects {

  class ProjectPath private (val value: String) extends AnyVal with RelativePathTinyType
  implicit object ProjectPath extends TinyTypeFactory[ProjectPath](new ProjectPath(_)) with RelativePath {
    addConstraint(
      check   = value => value.contains("/") && (value.indexOf("/") == value.lastIndexOf("/")),
      message = (value: String) => s"'$value' is not a valid $typeName"
    )
  }

  class FullProjectPath private (val value: String) extends AnyVal with StringTinyType
  implicit object FullProjectPath extends TinyTypeFactory[FullProjectPath](new FullProjectPath(_)) with Url {
    private val projectPathValidator = "^http[s]?:\\/\\/.*\\/projects\\/.*\\/.*$"
    addConstraint(
      _ matches projectPathValidator,
      message = (value: String) => s"'$value' is not a valid $typeName"
    )

    def apply(renkuBaseUrl: RenkuBaseUrl, projectPath: ProjectPath): FullProjectPath =
      FullProjectPath((renkuBaseUrl / "projects" / projectPath).value)

    private val pathExtractor = "^.*\\/projects\\/(.*\\/.*)$".r
    implicit lazy val projectPathConverter: TinyTypeConverter[FullProjectPath, ProjectPath] = {
      case FullProjectPath(pathExtractor(path)) => ProjectPath.from(path)
      case illegalValue                         => Left(new IllegalArgumentException(s"'$illegalValue' cannot be converted to a ProjectPath"))
    }
  }

  final class Name private (val value: String) extends AnyVal with StringTinyType
  implicit object Name extends TinyTypeFactory[Name](new Name(_)) with NonBlank

  final class DateCreated private (val value: Instant) extends AnyVal with InstantTinyType
  implicit object DateCreated extends TinyTypeFactory[DateCreated](new DateCreated(_)) with InstantNotInTheFuture

  final class FilePath private (val value: String) extends AnyVal with RelativePathTinyType
  object FilePath extends TinyTypeFactory[FilePath](new FilePath(_)) with RelativePath with RelativePathOps[FilePath]
}