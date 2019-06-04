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
import ch.datascience.tinytypes.{TinyType, TinyTypeFactory}
import ch.datascience.webhookservice.audit.AuditLogConfig._
import ch.epfl.dedis.lib.darc.{Signer, SignerFactory}
import com.typesafe.config.{Config, ConfigFactory}
import eu.timepit.refined.W
import eu.timepit.refined.api.Refined
import eu.timepit.refined.string.MatchesRegex
import pureconfig.ConfigReader
import pureconfig.error.CannotConvert

import scala.language.higherKinds
import scala.util.Try

case class AuditLogConfig(topic: Topic, serversFilename: ServersFilename, signers: AuditLogSigners)

case class AuditLogSigners(admin: AdminSigner, user: UserSigner)

object AuditLogConfig {

  import ch.datascience.config.ConfigLoader._
  import eu.timepit.refined.pureconfig._

  type Topic           = String Refined MatchesRegex[W.`"""^(?!\\s*$).+"""`.T]
  type ServersFilename = String Refined MatchesRegex[W.`"""^(?!\\s*$).+"""`.T]
  class AdminSigner private (val value: Signer) extends AnyVal with TinyType[Signer]
  object AdminSigner
      extends TinyTypeFactory[Signer, AdminSigner](new AdminSigner(_))
      with SignerConfigReader[AdminSigner]
  class UserSigner private (val value: Signer) extends AnyVal with TinyType[Signer]
  object UserSigner extends TinyTypeFactory[Signer, UserSigner](new UserSigner(_)) with SignerConfigReader[UserSigner]

  trait SignerConfigReader[T <: TinyType[Signer]] {
    self: TinyTypeFactory[Signer, T] =>

    import java.util.Base64.{getDecoder => base64Decoder}

    implicit val signerReader: ConfigReader[T] = {
      ConfigReader.fromString[T] { signerSecret =>
        Try(SignerFactory.New(base64Decoder.decode(signerSecret.getBytes("utf-8")))).toEither
          .flatMap(signer => from(signer))
          .leftMap { exception =>
            CannotConvert(signerSecret, typeName, exception.toString)
          }
      }
    }
  }

  def get[Interpretation[_]](
      config:    Config = ConfigFactory.load()
  )(implicit ME: MonadError[Interpretation, Throwable]): OptionT[Interpretation, AuditLogConfig] = OptionT {
    find[Interpretation, Boolean]("audit-log.enabled", config) flatMap {
      case false => ME.pure(Option.empty[AuditLogConfig])
      case true  => readConfigValues[Interpretation](config)
    }
  }

  private def readConfigValues[Interpretation[_]](
      config:    Config
  )(implicit ME: MonadError[Interpretation, Throwable]) =
    for {
      topic           <- find[Interpretation, Topic]("audit-log.topic", config)
      serversFilename <- find[Interpretation, ServersFilename]("audit-log.servers-filename", config)
      signers         <- readSigners[Interpretation](config)
    } yield Option(AuditLogConfig(topic, serversFilename, signers))

  private def readSigners[Interpretation[_]](
      config:    Config
  )(implicit ME: MonadError[Interpretation, Throwable]) =
    for {
      admin <- find[Interpretation, AdminSigner]("audit-log.secrets.admin", config)
      user  <- find[Interpretation, UserSigner]("audit-log.secrets.user", config)
    } yield AuditLogSigners(admin, user)
}
