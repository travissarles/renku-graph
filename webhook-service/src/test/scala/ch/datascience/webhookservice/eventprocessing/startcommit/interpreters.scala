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

package ch.datascience.webhookservice.eventprocessing.startcommit

import cats.effect.IO
import cats.implicits._
import ch.datascience.graph.tokenrepository.AccessTokenFinder
import ch.datascience.logging.ExecutionTimeRecorder
import ch.datascience.webhookservice.commits.CommitInfoFinder
import ch.datascience.webhookservice.eventprocessing.commitevent.CommitEventSender
import io.chrisdavenport.log4cats.Logger

import scala.util.Try

class TryCommitToEventLog(
    accessTokenFinder:     AccessTokenFinder[Try],
    commitEventsSource:    CommitEventsSourceBuilder[Try],
    commitEventSender:     CommitEventSender[Try],
    logger:                Logger[Try],
    executionTimeRecorder: ExecutionTimeRecorder[Try]
) extends CommitToEventLog[Try](accessTokenFinder,
                                  commitEventsSource,
                                  commitEventSender,
                                  logger,
                                  executionTimeRecorder)
class IOCommitToEventLog(
    accessTokenFinder:     AccessTokenFinder[IO],
    commitEventsSource:    CommitEventsSourceBuilder[IO],
    commitEventSender:     CommitEventSender[IO],
    logger:                Logger[IO],
    executionTimeRecorder: ExecutionTimeRecorder[IO]
) extends CommitToEventLog[IO](accessTokenFinder,
                                 commitEventsSource,
                                 commitEventSender,
                                 logger,
                                 executionTimeRecorder)

private class TryCommitEventsSourceBuilder(
    commitInfoFinder: CommitInfoFinder[Try]
) extends CommitEventsSourceBuilder[Try](commitInfoFinder)
