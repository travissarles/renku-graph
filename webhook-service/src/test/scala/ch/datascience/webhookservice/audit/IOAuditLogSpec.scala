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

package ch.datascience.webhookservice.audit

import cats.data.OptionT
import cats.effect.IO
import ch.datascience.interpreters.TestLogger
import ch.datascience.interpreters.TestLogger.Level.Info
import org.scalatest.Matchers._
import org.scalatest.WordSpec

class IOAuditLogSpec extends WordSpec {

  "apply" should {

    "return NoAuditLog if Audit Log is disabled in the config" in {
      val logger = TestLogger[IO]()

      IOAuditLog(OptionT.none[IO, AuditLogConfig], logger).unsafeRunSync() shouldBe AuditLogPassThrough

      logger.loggedOnly(Info("SecureKG auditing disabled"))
    }
  }
}
