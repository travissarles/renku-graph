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

package io.renku.eventlog.init

import cats.implicits._
import doobie.implicits._
import doobie.util.fragment.Fragment
import io.renku.eventlog.InMemoryEventLogDb
import org.scalatest.Suite

trait DbInitSpec extends InMemoryEventLogDb {
  self: Suite =>

  protected def tableExists(): Boolean =
    sql"""select exists (select * from event_log);"""
      .query[Boolean]
      .option
      .transact(transactor.get)
      .recover { case _ => None }
      .unsafeRunSync()
      .isDefined

  protected def createTable(): Unit = execute {
    sql"""
         |CREATE TABLE event_log(
         | event_id varchar NOT NULL,
         | project_id int4 NOT NULL,
         | status varchar NOT NULL,
         | created_date timestamp NOT NULL,
         | execution_date timestamp NOT NULL,
         | event_date timestamp NOT NULL,
         | event_body text NOT NULL,
         | message varchar,
         | PRIMARY KEY (event_id, project_id)
         |);
       """.stripMargin.update.run.map(_ => ())
  }

  protected def dropTable(): Unit = execute {
    sql"DROP TABLE IF EXISTS event_log".update.run.map(_ => ())
  }

  protected def verifyTrue(sql: Fragment): Unit = execute {
    sql.update.run.map(_ => ())
  }
}
