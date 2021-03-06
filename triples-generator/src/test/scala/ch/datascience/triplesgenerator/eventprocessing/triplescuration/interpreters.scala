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

package ch.datascience.triplesgenerator.eventprocessing.triplescuration

import cats.MonadError
import ch.datascience.triplesgenerator.eventprocessing.triplescuration.forks.ForkInfoUpdater
import ch.datascience.triplesgenerator.eventprocessing.triplescuration.persondetails.PersonDetailsUpdater

import scala.util.Try

object interpreters {
  abstract class TryTriplesCurator(
      personDetailsUpdater: PersonDetailsUpdater[Try],
      forkInfoUpdater:      ForkInfoUpdater[Try]
  )(implicit ME:            MonadError[Try, Throwable])
      extends TriplesCurator[Try](personDetailsUpdater, forkInfoUpdater)
}
