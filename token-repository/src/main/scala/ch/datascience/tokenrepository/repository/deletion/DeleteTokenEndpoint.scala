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

package ch.datascience.tokenrepository.repository.deletion

import cats.MonadError
import cats.effect.{ContextShift, Effect, IO}
import cats.implicits._
import ch.datascience.controllers.ErrorMessage
import ch.datascience.controllers.ErrorMessage._
import ch.datascience.graph.model.events.ProjectId
import ch.datascience.logging.ApplicationLogger
import ch.datascience.tokenrepository.repository.ProjectIdPathBinder
import io.chrisdavenport.log4cats.Logger
import org.http4s.dsl.Http4sDsl
import org.http4s.{HttpRoutes, Response}

import scala.language.higherKinds
import scala.util.control.NonFatal

class DeleteTokenEndpoint[Interpretation[_]: Effect](
    tokenRemover: TokenRemover[Interpretation],
    logger:       Logger[Interpretation]
)(implicit ME:    MonadError[Interpretation, Throwable])
    extends Http4sDsl[Interpretation] {

  val deleteToken: HttpRoutes[Interpretation] = HttpRoutes.of[Interpretation] {
    case DELETE -> Root / "projects" / ProjectIdPathBinder(projectId) / "tokens" =>
      tokenRemover
        .delete(projectId)
        .flatMap(toHttpResult(projectId))
        .recoverWith(httpResult(projectId))
  }

  private def toHttpResult(projectId: ProjectId): Unit => Interpretation[Response[Interpretation]] = _ => {
    logger.info(s"Token deleted for projectId: $projectId")
    NoContent()
  }

  private def httpResult(projectId: ProjectId): PartialFunction[Throwable, Interpretation[Response[Interpretation]]] = {
    case NonFatal(exception) =>
      val errorMessage = ErrorMessage(s"Deleting token for projectId: $projectId failed")
      logger.error(exception)(errorMessage.value)
      InternalServerError(errorMessage)
  }
}

class IODeleteTokenEndpoint(implicit contextShift: ContextShift[IO])
    extends DeleteTokenEndpoint[IO](new IOTokenRemover, ApplicationLogger)