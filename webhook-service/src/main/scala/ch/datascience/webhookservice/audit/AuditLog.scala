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

import java.util

import cats.data.OptionT
import cats.effect.IO
import ch.datascience.graph.model.events.SerializedCommitEvent
import ch.datascience.logging.ApplicationLogger
import ch.epfl.dedis.byzcoin.SignerCounters
import ch.epfl.dedis.eventlog.EventLogInstance
import ch.epfl.dedis.lib.darc.Signer
import io.chrisdavenport.log4cats.Logger

import scala.language.higherKinds

trait AuditLog[Interpretation[_]] {
  def push(serializedEvent: SerializedCommitEvent): Interpretation[Unit]
}

object AuditLogPassThrough extends AuditLog[IO] {
  override def push(serializedEvent: SerializedCommitEvent): IO[Unit] = IO.unit
}

class IOAuditLog private (config:         AuditLogConfig,
                          eventLog:       EventLogInstance,
                          signers:        util.List[Signer],
                          signerCounters: SignerCounters)
    extends AuditLog[IO] {
  override def push(serializedEvent: SerializedCommitEvent): IO[Unit] = IO {
    import ch.epfl.dedis.eventlog.Event

    val event = new Event(config.topic.value, serializedEvent.value)
    val incrementedSignerCounters = {
      signerCounters.increment()
      signerCounters.getCounters
    }

    eventLog.log(event, signers, incrementedSignerCounters)
  }
}

object IOAuditLog {
  import java.time.Duration
  import java.time.temporal.ChronoUnit.MILLIS

  import ch.epfl.dedis.byzcoin.ByzCoinRPC
  import ch.epfl.dedis.eventlog.EventLogInstance
  import ch.epfl.dedis.lib.darc._
  import ch.epfl.dedis.lib.network.{Roster, ServerIdentity}

  import scala.collection.JavaConverters._

  def apply(maybeConfig: OptionT[IO, AuditLogConfig] = AuditLogConfig.get(),
            logger:      Logger[IO]                  = ApplicationLogger): IO[AuditLog[IO]] =
    maybeConfig
      .semiflatMap(instantiateAuditLog)
      .getOrElse {
        logger.info("SecureKG auditing disabled")
        AuditLogPassThrough
      }

  private def instantiateAuditLog(config: AuditLogConfig) = IO {
    val admin: Signer = new SignerEd25519
    val roster = {
      val identities = List.empty[ServerIdentity]
      val conodes    = identities.take(4)
      new Roster(conodes.asJava)
    }
    val genesisDarc: Darc = {
      val darc = ByzCoinRPC.makeGenesisDarc(admin, roster)
      darc.addIdentity("spawn:eventlog", admin.getIdentity, Rules.OR)
      darc.addIdentity("invoke:" + EventLogInstance.ContractId + "." + EventLogInstance.LogCmd,
                       admin.getIdentity,
                       Rules.OR)
      darc
    }
    val byzcoin        = new ByzCoinRPC(roster, genesisDarc, Duration.of(1000, MILLIS))
    val signerCounters = byzcoin.getSignerCounters(List(admin.getIdentity.toString).asJava)
    val signers        = List(admin).asJava
    val eventLog = new EventLogInstance(byzcoin,
                                        genesisDarc.getId,
                                        signers,
                                        List(new java.lang.Long(signerCounters.head + 1)).asJava)

    new IOAuditLog(config, eventLog, signers, signerCounters)
  }
}
