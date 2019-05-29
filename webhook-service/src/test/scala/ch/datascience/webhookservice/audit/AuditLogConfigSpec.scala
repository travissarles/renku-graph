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

import cats.implicits._
import ch.datascience.generators.Generators.Implicits._
import ch.datascience.generators.Generators._
import com.typesafe.config.ConfigFactory
import org.scalatest.Matchers._
import org.scalatest.WordSpec

import scala.collection.JavaConverters._
import scala.util.{Failure, Success, Try}

class AuditLogConfigSpec extends WordSpec {

  "apply" should {

    "return an instance of the config if 'audit-log.enabled' is true " +
      "and there are all required entries" in {
      val topic           = nonEmptyStrings().generateOne
      val serversFilename = nonEmptyStrings().generateOne

      val config = ConfigFactory.parseMap(
        Map(
          "audit-log" -> Map(
            "enabled"          -> "true",
            "topic"            -> topic,
            "servers-filename" -> serversFilename
          ).asJava
        ).asJava
      )

      val Success(Some(auditLogConfig)) = AuditLogConfig.get[Try](config).value

      auditLogConfig.topic.value           shouldBe topic
      auditLogConfig.serversFilename.value shouldBe serversFilename
    }

    "return None if 'audit-log.enabled' is false" in {
      val config = ConfigFactory.parseMap(
        Map(
          "audit-log" -> Map(
            "enabled" -> "false"
          ).asJava
        ).asJava
      )

      AuditLogConfig.get[Try](config).value shouldBe Success(Option.empty)
    }

    "fail if there's no 'audit-log.enabled'" in {
      val config = ConfigFactory.empty()

      AuditLogConfig.get[Try](config).value shouldBe a[Failure[_]]
    }

    "fail if 'audit-log.enabled' is true but no 'topic'" in {
      val config = ConfigFactory.parseMap(
        Map(
          "audit-log" -> Map(
            "enabled" -> "true"
          ).asJava
        ).asJava
      )

      AuditLogConfig.get[Try](config).value shouldBe a[Failure[_]]
    }

    "fail if 'audit-log.enabled' is true but no 'servers-filename'" in {
      val config = ConfigFactory.parseMap(
        Map(
          "audit-log" -> Map(
            "enabled" -> "true",
            "topic"   -> nonEmptyStrings().generateOne
          ).asJava
        ).asJava
      )

      AuditLogConfig.get[Try](config).value shouldBe a[Failure[_]]
    }
  }
}
