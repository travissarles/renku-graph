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

import cats.effect.IO
import cats.implicits._
import cats.{Monad, MonadError}
import ch.datascience.graph.model.events.CommitEvent
import javax.inject.{Inject, Singleton}

import scala.language.higherKinds

class CommitEventSender[Interpretation[_]: Monad](
    eventLog:              EventLog[Interpretation],
    commitEventSerializer: CommitEventSerializer[Interpretation]
)(implicit ME:             MonadError[Interpretation, Throwable]) {

  import commitEventSerializer._

  def send(commitEvent: CommitEvent): Interpretation[Unit] =
    for {
      serialisedEvent <- serialiseToJsonString(commitEvent)
      _               <- eventLog.append(serialisedEvent)
    } yield ()
}

@Singleton
class IOCommitEventSender @Inject()(
    eventLog:              IOEventLog,
    commitEventSerializer: IOCommitEventSerializer,
) extends CommitEventSender[IO](eventLog, commitEventSerializer)