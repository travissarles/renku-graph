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

import java.nio.file.Paths

import ch.datascience.generators.Generators._
import ch.datascience.webhookservice.audit.AuditLogConfig._
import ch.epfl.dedis.lib.darc.{Signer, SignerEd25519}
import eu.timepit.refined.api.RefType
import org.scalacheck.Gen

package object audit {

  private implicit val signers: Gen[Signer] = Gen.uuid map (_ => new SignerEd25519)

  implicit val adminSecrets: Gen[AdminSigner] = signers.map(AdminSigner.apply)
  implicit val userSecrets:  Gen[UserSigner]  = signers.map(UserSigner.apply)
  implicit val auditLogSecrets: Gen[AuditLogSigners] = for {
    adminSecret <- adminSecrets
    userSecret  <- userSecrets
  } yield AuditLogSigners.apply(adminSecret, userSecret)

  implicit val auditLogConfigs: Gen[AuditLogConfig] = for {
    topic            <- nonEmptyStrings() map (RefType.applyRef[Topic](_).getOrError)
    serverConfigFile <- relativePaths() map (path => Paths.get(path)) map ServersConfigFile.apply
    secrets          <- auditLogSecrets
  } yield AuditLogConfig(topic, serverConfigFile, secrets)

  private implicit class RefinedOps[V](maybeValue: Either[String, V]) {
    lazy val getOrError: V = maybeValue.fold(s => throw new IllegalArgumentException(s), identity)
  }
}
