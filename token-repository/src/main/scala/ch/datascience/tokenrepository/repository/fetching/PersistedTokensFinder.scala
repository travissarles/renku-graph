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

package ch.datascience.tokenrepository.repository.fetching

import cats.data.OptionT
import cats.effect.{Bracket, ContextShift, IO}
import cats.implicits._
import ch.datascience.db.DbTransactor
import ch.datascience.graph.model.projects.{Id, Path}
import ch.datascience.tokenrepository.repository.AccessTokenCrypto.EncryptedAccessToken
import ch.datascience.tokenrepository.repository.ProjectsTokensDB

import scala.language.higherKinds

private class PersistedTokensFinder[Interpretation[_]](
    transactor: DbTransactor[Interpretation, ProjectsTokensDB]
)(implicit ME:  Bracket[Interpretation, Throwable]) {

  import doobie.implicits._

  def findToken(projectId: Id): OptionT[Interpretation, EncryptedAccessToken] = OptionT {
    sql"select token from projects_tokens where project_id = ${projectId.value}"
      .query[String]
      .option
      .transact(transactor.get)
      .flatMap(toSerializedAccessToken)
  }

  def findToken(projectPath: Path): OptionT[Interpretation, EncryptedAccessToken] = OptionT {
    sql"select token from projects_tokens where project_path = ${projectPath.value}"
      .query[String]
      .option
      .transact(transactor.get)
      .flatMap(toSerializedAccessToken)
  }

  private lazy val toSerializedAccessToken: Option[String] => Interpretation[Option[EncryptedAccessToken]] = {
    case None => ME.pure(None)
    case Some(encryptedToken) =>
      ME.fromEither {
        EncryptedAccessToken.from(encryptedToken).map(Option.apply)
      }
  }
}

private class IOPersistedTokensFinder(
    transactor:          DbTransactor[IO, ProjectsTokensDB]
)(implicit contextShift: ContextShift[IO])
    extends PersistedTokensFinder[IO](transactor)
