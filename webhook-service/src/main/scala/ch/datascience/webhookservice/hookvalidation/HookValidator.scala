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

package ch.datascience.webhookservice.hookvalidation

import cats.MonadError
import cats.effect.{ContextShift, IO, Timer}
import cats.implicits._
import ch.datascience.config.GitLab
import ch.datascience.control.Throttler
import ch.datascience.graph.config.GitLabUrl
import ch.datascience.graph.model.projects.{Id, Visibility}
import ch.datascience.graph.tokenrepository.{AccessTokenFinder, IOAccessTokenFinder, TokenRepositoryUrl}
import ch.datascience.http.client.AccessToken
import ch.datascience.http.client.RestClientError.UnauthorizedException
import ch.datascience.logging.ApplicationLogger
import ch.datascience.webhookservice.hookvalidation.ProjectHookVerifier.HookIdentifier
import ch.datascience.webhookservice.project._
import ch.datascience.webhookservice.tokenrepository._
import io.chrisdavenport.log4cats.Logger

import scala.concurrent.ExecutionContext
import scala.language.higherKinds
import scala.util.control.NonFatal

class HookValidator[Interpretation[_]](
    projectHookUrl:        ProjectHookUrl,
    projectInfoFinder:     ProjectInfoFinder[Interpretation],
    projectHookVerifier:   ProjectHookVerifier[Interpretation],
    accessTokenFinder:     AccessTokenFinder[Interpretation],
    accessTokenAssociator: AccessTokenAssociator[Interpretation],
    accessTokenRemover:    AccessTokenRemover[Interpretation],
    logger:                Logger[Interpretation]
)(implicit ME:             MonadError[Interpretation, Throwable]) {

  import HookValidator.HookValidationResult._
  import HookValidator._
  import IOAccessTokenFinder._
  import Token._
  import Visibility._
  import accessTokenAssociator._
  import accessTokenFinder._
  import accessTokenRemover._
  import projectHookVerifier._
  import projectInfoFinder._

  def validateHook(projectId: Id, maybeAccessToken: Option[AccessToken]): Interpretation[HookValidationResult] =
    findVisibilityAndToken(projectId, maybeAccessToken) flatMap {
      case (Public, token) =>
        for {
          hookPresent      <- checkHookPresence(HookIdentifier(projectId, projectHookUrl), token.value)
          validationResult <- toValidationResult(hookPresent, projectId)
        } yield validationResult
      case (_: TokenProtected, GivenToken(token)) =>
        for {
          hookPresent      <- checkHookPresence(HookIdentifier(projectId, projectHookUrl), token)
          _                <- if (hookPresent) associate(projectId, token) else ME.unit
          _                <- if (!hookPresent) removeAccessToken(projectId) else ME.unit
          validationResult <- toValidationResult(hookPresent, projectId)
        } yield validationResult
      case (_: TokenProtected, StoredToken(token)) =>
        for {
          hookPresent      <- checkHookPresence(HookIdentifier(projectId, projectHookUrl), token)
          _                <- if (!hookPresent) removeAccessToken(projectId) else ME.unit
          validationResult <- toValidationResult(hookPresent, projectId)
        } yield validationResult
    } recoverWith loggingError(projectId)

  private def findVisibilityAndToken(
      projectId:        Id,
      maybeAccessToken: Option[AccessToken]
  ): Interpretation[(Visibility, Token)] =
    maybeAccessToken match {
      case None =>
        findVisibilityAndStoredToken(projectId)
      case Some(accessToken) =>
        findProjectInfo(projectId, maybeAccessToken)
          .map(_.visibility -> (GivenToken(accessToken): Token))
          .recoverWith(visibilityAndStoredToken(projectId))
    }

  private def visibilityAndStoredToken(
      projectId: Id
  ): PartialFunction[Throwable, Interpretation[(Visibility, Token)]] = {
    case UnauthorizedException => findVisibilityAndStoredToken(projectId)
  }

  private def findVisibilityAndStoredToken(projectId: Id) = {
    for {
      storedAccessToken <- findAccessToken(projectId) flatMap getOrError(projectId)
      projectInfo       <- findProjectInfo(projectId, Some(storedAccessToken.value))
    } yield projectInfo.visibility -> storedAccessToken
  } recoverWith storedAccessTokenError(projectId)

  private def getOrError(projectId: Id): Option[AccessToken] => Interpretation[Token] = {
    case Some(token) => ME.pure(StoredToken(token))
    case None        => ME.raiseError[Token](NoAccessTokenException(s"No access token found for projectId $projectId"))
  }

  private def storedAccessTokenError(
      projectId: Id
  ): PartialFunction[Throwable, Interpretation[(Visibility, Token)]] = {
    case UnauthorizedException => ME.raiseError(new Exception(s"Stored access token for $projectId is invalid"))
  }

  private def toValidationResult(projectHookPresent: Boolean, projectId: Id): Interpretation[HookValidationResult] =
    if (projectHookPresent)
      ME.pure(HookExists)
    else
      ME.pure(HookMissing)

  private def loggingError(projectId: Id): PartialFunction[Throwable, Interpretation[HookValidationResult]] = {
    case exception @ NoAccessTokenException(message) =>
      logger.info(s"Hook validation failed: $message")
      ME.raiseError(exception)
    case NonFatal(exception) =>
      logger.error(exception)(s"Hook validation failed for project with id $projectId")
      ME.raiseError(exception)
  }

  private sealed abstract class Token(val value: AccessToken)
  private object Token {
    case class GivenToken(override val value:  AccessToken) extends Token(value)
    case class StoredToken(override val value: AccessToken) extends Token(value)
  }
}

object HookValidator {

  sealed trait HookValidationResult
  object HookValidationResult {
    final case object HookExists  extends HookValidationResult
    final case object HookMissing extends HookValidationResult
  }

  final case class NoAccessTokenException(message: String) extends RuntimeException(message)
}

object IOHookValidator {
  def apply(
      projectHookUrl:          ProjectHookUrl,
      gitLabThrottler:         Throttler[IO, GitLab]
  )(implicit executionContext: ExecutionContext,
    contextShift:              ContextShift[IO],
    timer:                     Timer[IO]): IO[HookValidator[IO]] =
    for {
      tokenRepositoryUrl <- TokenRepositoryUrl[IO]()
      gitLabUrl          <- GitLabUrl[IO]()
    } yield new HookValidator[IO](
      projectHookUrl,
      new IOProjectInfoFinder(gitLabUrl, gitLabThrottler, ApplicationLogger),
      new IOProjectHookVerifier(gitLabUrl, gitLabThrottler, ApplicationLogger),
      new IOAccessTokenFinder(tokenRepositoryUrl, ApplicationLogger),
      new IOAccessTokenAssociator(tokenRepositoryUrl, ApplicationLogger),
      new IOAccessTokenRemover(tokenRepositoryUrl, ApplicationLogger),
      ApplicationLogger
    )
}
