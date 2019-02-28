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

package ch.datascience.webhookservice.eventprocessing.commitevent

import java.nio.file.{Files, OpenOption, Path, StandardOpenOption}

import cats.MonadError
import cats.effect.IO
import javax.inject.{Inject, Named, Singleton}

import scala.collection.JavaConverters._
import scala.language.higherKinds
import scala.util.Try

class FileEventLog[Interpretation[_]](
    eventLogFilePath: Path
)(implicit ME:        MonadError[Interpretation, Throwable])
    extends EventLog[Interpretation] {

  override def append(line: String): Interpretation[Unit] = ME.fromTry {
    Try {
      Files.write(eventLogFilePath, Seq(line).asJava, openOptions: _*)
    }
  }

  private val openOptions: Seq[OpenOption] = Seq(
    StandardOpenOption.WRITE,
    StandardOpenOption.CREATE,
    StandardOpenOption.APPEND,
    StandardOpenOption.SYNC
  )
}

@Singleton
class FileIOEventLog @Inject()(
    @Named("event-log-file-path") eventLogFilePath: Path
) extends FileEventLog[IO](eventLogFilePath)
    with IOEventLog // this mixin is just to make Guice be able to inject EventLog[IO]