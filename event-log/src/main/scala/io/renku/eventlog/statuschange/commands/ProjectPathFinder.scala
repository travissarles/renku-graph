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

package io.renku.eventlog.statuschange.commands

import cats.effect.Bracket
import ch.datascience.db.DbTransactor
import ch.datascience.graph.model.events.CompoundEventId
import ch.datascience.graph.model.projects
import doobie.implicits._
import io.renku.eventlog.EventLogDB

import scala.language.higherKinds

private object ProjectPathFinder {

  import io.renku.eventlog.TypesSerializers._

  def findProjectPath[Interpretation[_]](
      eventId:           CompoundEventId
  )(implicit transactor: DbTransactor[Interpretation, EventLogDB], ME: Bracket[Interpretation, Throwable]) =
    sql"""|select project_path
          |from event_log 
          |where event_id = ${eventId.id} and project_id = ${eventId.projectId}
          |""".stripMargin
      .query[projects.Path]
      .unique
      .transact(transactor.get)
}
