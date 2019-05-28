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

import cats.MonadError
import cats.data.OptionT
import cats.implicits._
import ch.datascience.webhookservice.audit.AuditLogConfig.Topic
import com.typesafe.config.{Config, ConfigFactory}
import eu.timepit.refined.W
import eu.timepit.refined.api.Refined
import eu.timepit.refined.string.MatchesRegex

import scala.language.higherKinds

case class AuditLogConfig(topic: Topic)

object AuditLogConfig {

  import ch.datascience.config.ConfigLoader._
  import eu.timepit.refined.pureconfig._

  type Topic = String Refined MatchesRegex[W.`"""^(?!\\s*$).+"""`.T]

  def get[Interpretation[_]](
      config:    Config = ConfigFactory.load()
  )(implicit ME: MonadError[Interpretation, Throwable]): OptionT[Interpretation, AuditLogConfig] = OptionT {
    find[Interpretation, Boolean]("audit-log.enabled", config) flatMap {
      case true  => find[Interpretation, Topic]("audit-log.topic", config) map AuditLogConfig.apply map Option.apply
      case false => ME.pure(Option.empty[AuditLogConfig])
    }
  }
}
