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

import cats.MonadError
import cats.implicits._
import ch.datascience.generators.Generators.Implicits._
import ch.datascience.graph.model.events.EventsGenerators._
import ch.datascience.graph.model.events.SerializedCommitEvent
import ch.datascience.webhookservice.audit.AuditLog
import org.scalamock.scalatest.MockFactory
import org.scalatest.Matchers._
import org.scalatest.WordSpec

import scala.util.Try

class AuditLogPushSpec extends WordSpec with MockFactory {

  "pushToAuditLog" should {

    "be successful if sending the given Commit Event to the AuditLog succeed" in new TestCase {
      val serializedEvent = serializedCommitEvents.generateOne

      (auditLog
        .push(_: SerializedCommitEvent))
        .expects(serializedEvent)
        .returning(context.unit)

      auditLogPush.pushToAuditLog(serializedEvent) shouldBe context.unit
    }
  }

  private trait TestCase {
    val context = MonadError[Try, Throwable]

    val auditLog     = mock[AuditLog[Try]]
    val auditLogPush = new AuditLogPush[Try](auditLog)
  }
}