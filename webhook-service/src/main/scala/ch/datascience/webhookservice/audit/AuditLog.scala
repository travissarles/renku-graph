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

import java.time.Instant

import cats.data.OptionT
import cats.effect.IO
import ch.datascience.graph.model.events.SerializedCommitEvent
import ch.datascience.logging.ApplicationLogger
import ch.datascience.webhookservice.audit.AuditLogConfig.ServersConfigFile
import ch.datascience.webhookservice.audit.IOAuditLog.EventLog
import ch.epfl.dedis.byzcoin.{InstanceId, SignerCounters}
import ch.epfl.dedis.eventlog.{Event, SearchResponse}
import ch.epfl.dedis.lib.darc.Signer
import ch.epfl.dedis.lib.network.ServerToml
import com.moandjiezana.toml.Toml
import io.chrisdavenport.log4cats.Logger

import scala.collection.JavaConverters._
import scala.language.higherKinds

trait AuditLog[Interpretation[_]] {
  def push(serializedEvent: SerializedCommitEvent): Interpretation[Unit]
  def fetchAllEvents: Interpretation[List[SerializedCommitEvent]]
}

object AuditLogPassThrough extends AuditLog[IO] {
  override def push(serializedEvent: SerializedCommitEvent): IO[Unit] = IO.unit
  override def fetchAllEvents: IO[List[SerializedCommitEvent]] = IO.pure(List.empty)
}

class IOAuditLog private[audit] (config:         AuditLogConfig,
                                 eventLog:       EventLog,
                                 signer:         Signer,
                                 signerCounters: SignerCounters,
                                 now:            () => Instant = () => Instant.now())
    extends AuditLog[IO] {

  import cats.implicits._
  import config._

  private lazy val Epoch: Long = Instant.EPOCH.toNanos
  private[this] val signers = List(signer).asJava

  override def push(serializedEvent: SerializedCommitEvent): IO[Unit] = IO {
    import ch.epfl.dedis.eventlog.Event

    signerCounters.increment()

    val event = new Event(topic.value, serializedEvent.value)

    eventLog.log(event, signers, signerCounters.getCounters)
  }

  override def fetchAllEvents: IO[List[SerializedCommitEvent]] =
    for {
      auditEvents <- IO(eventLog.search(topic.value, Epoch, now().toNanos).events.asScala.toList)
      serializedCommitEvents <- auditEvents
                                 .map(_.getContent)
                                 .map(SerializedCommitEvent.from)
                                 .map(IO.fromEither)
                                 .sequence
    } yield serializedCommitEvents

  private implicit class InstantOps(instant: Instant) {
    lazy val toNanos: Long = instant.toEpochMilli * 1000 * 1000
  }
}

object IOAuditLog {
  import java.nio.file.Files.{newInputStream => fromInputStream}
  import java.time.Duration
  import java.time.temporal.ChronoUnit.MILLIS

  import ch.epfl.dedis.byzcoin.ByzCoinRPC
  import ch.epfl.dedis.eventlog.EventLogInstance
  import ch.epfl.dedis.eventlog.EventLogInstance._
  import ch.epfl.dedis.lib.darc._
  import ch.epfl.dedis.lib.network.{Roster, ServerIdentity}

  private[audit] class EventLog(instance: EventLogInstance) {
    def log(event: Event, signers: java.util.List[Signer], signerCtrs: java.util.List[java.lang.Long]): InstanceId =
      instance.log(event, signers, signerCtrs)
    def search(topic: String, from: Long, to: Long): SearchResponse =
      instance.search(topic, from, to)
  }

  def apply(maybeConfig: OptionT[IO, AuditLogConfig] = AuditLogConfig.get(),
            logger:      Logger[IO]                  = ApplicationLogger): IO[AuditLog[IO]] =
    maybeConfig
      .semiflatMap(instantiateAuditLog)
      .getOrElse {
        logger.info("SecureKG auditing disabled")
        AuditLogPassThrough
      }

  private def instantiateAuditLog(config: AuditLogConfig) =
    for {
      roster         <- readServerIdentities(config.serversConfigFile) map (_.take(4)) map (_.asJava) map (new Roster(_))
      genesisDarc    <- createGenesisDarc(config.signers, roster)
      byzcoin        <- IO(new ByzCoinRPC(roster, genesisDarc, Duration.of(1000, MILLIS)))
      signerCounters <- IO(byzcoin.getSignerCounters(List(config.signers.user.value.getIdentity.toString).asJava))
      eventLog       <- createEventLog(byzcoin, genesisDarc, config.signers.user.value, signerCounters)
    } yield new IOAuditLog(config, eventLog, config.signers.user.value, signerCounters)

  private def createEventLog(byzcoin: ByzCoinRPC, genesisDarc: Darc, user: Signer, signerCounters: SignerCounters) =
    IO {
      signerCounters.increment()
      new EventLog(
        new EventLogInstance(byzcoin, genesisDarc.getId, List(user).asJava, signerCounters.getCounters)
      )
    }

  private def createGenesisDarc(signers: AuditLogSigners, roster: Roster): IO[Darc] = IO {
    val darc = ByzCoinRPC.makeGenesisDarc(signers.admin.value, roster)
    darc.addIdentity(s"spawn:$ContractId", signers.user.value.getIdentity, Rules.OR)
    darc.addIdentity(s"invoke:$ContractId.$LogCmd", signers.user.value.getIdentity, Rules.OR)
    darc
  }

  private def readServerIdentities(serversFilename: ServersConfigFile): IO[List[ServerIdentity]] = IO {
    new Toml()
      .read(fromInputStream(serversFilename.value))
      .getTables("servers")
      .asScala
      .toList
      .map(_.to(classOf[ServerToml]))
      .map(new ServerIdentity(_))
  }
}
