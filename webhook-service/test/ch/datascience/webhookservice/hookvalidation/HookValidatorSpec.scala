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

package ch.datascience.webhookservice.hookvalidation

import ProjectHookVerifier.HookIdentifier
import cats.MonadError
import cats.implicits._
import ch.datascience.generators.CommonGraphGenerators._
import ch.datascience.generators.Generators.Implicits._
import ch.datascience.generators.Generators.exceptions
import ch.datascience.graph.model.events.ProjectId
import ch.datascience.http.client.AccessToken
import ch.datascience.interpreters.TestLogger
import ch.datascience.interpreters.TestLogger.Level.{Error, Info}
import ch.datascience.webhookservice.generators.ServiceTypesGenerators._
import ch.datascience.webhookservice.hookvalidation.HookValidator.HookValidationResult.{HookExists, HookMissing}
import ch.datascience.webhookservice.model.ProjectVisibility
import ch.datascience.webhookservice.project._
import io.chrisdavenport.log4cats.Logger
import org.scalacheck.Arbitrary
import org.scalamock.scalatest.MockFactory
import org.scalatest.Matchers._
import org.scalatest.WordSpec

import scala.util.{Failure, Try}

class HookValidatorSpec extends WordSpec with MockFactory {

  "validateHook" should {

    "succeed with HookExists if project is public and there's a hook for it" in new TestCase {

      val projectInfo = projectInfos.generateOne.copy(visibility = ProjectVisibility.Public)
      val projectId   = projectInfo.id

      (projectInfoFinder
        .findProjectInfo(_: ProjectId, _: AccessToken))
        .expects(projectId, accessToken)
        .returning(context.pure(projectInfo))

      val projectHookUrl = projectHookUrls.generateOne
      (projectHookUrlFinder.findProjectHookUrl _)
        .expects()
        .returning(context.pure(projectHookUrl))

      (projectHookVerifier
        .checkProjectHookPresence(_: HookIdentifier, _: AccessToken))
        .expects(HookIdentifier(projectId, projectHookUrl), accessToken)
        .returning(context.pure(true))

      validator.validateHook(projectId, accessToken) shouldBe context.pure(HookExists)

      logger.loggedOnly(Info(s"Hook exists for project with id $projectId"))
    }

    "succeed with HookMissing if project is public and there's no hook for it" in new TestCase {

      val projectInfo = projectInfos.generateOne.copy(visibility = ProjectVisibility.Public)
      val projectId   = projectInfo.id

      (projectInfoFinder
        .findProjectInfo(_: ProjectId, _: AccessToken))
        .expects(projectId, accessToken)
        .returning(context.pure(projectInfo))

      val projectHookUrl = projectHookUrls.generateOne
      (projectHookUrlFinder.findProjectHookUrl _)
        .expects()
        .returning(context.pure(projectHookUrl))

      (projectHookVerifier
        .checkProjectHookPresence(_: HookIdentifier, _: AccessToken))
        .expects(HookIdentifier(projectId, projectHookUrl), accessToken)
        .returning(context.pure(false))

      validator.validateHook(projectId, accessToken) shouldBe context.pure(HookMissing)

      logger.loggedOnly(Info(s"Hook missing for project with id $projectId"))
    }

    "throw NotImplementedError for a non-public project" in new TestCase {

      val projectInfo = projectInfos.generateOne.copy(
        visibility = projectVisibilities.generateDifferentThan(ProjectVisibility.Public)
      )
      val projectId = projectInfo.id

      (projectInfoFinder
        .findProjectInfo(_: ProjectId, _: AccessToken))
        .expects(projectId, accessToken)
        .returning(context.pure(projectInfo))

      val projectHookUrl = projectHookUrls.generateOne
      (projectHookUrlFinder.findProjectHookUrl _)
        .expects()
        .returning(context.pure(projectHookUrl))

      (projectHookVerifier
        .checkProjectHookPresence(_: HookIdentifier, _: AccessToken))
        .expects(HookIdentifier(projectId, projectHookUrl), accessToken)
        .returning(context.pure(Arbitrary.arbBool.arbitrary.generateOne))

      val Failure(exception) = validator.validateHook(projectId, accessToken)

      exception shouldBe an[NotImplementedError]

      logger.loggedOnly(Error(s"Hook validation fails for project with id $projectId", exception))
    }

    "log an error if project info fetching fails" in new TestCase {

      val projectInfo = projectInfos.generateOne
      val projectId   = projectInfo.id

      val exception: Exception    = exceptions.generateOne
      val error:     Try[Nothing] = context.raiseError(exception)
      (projectInfoFinder
        .findProjectInfo(_: ProjectId, _: AccessToken))
        .expects(projectId, accessToken)
        .returning(error)

      validator.validateHook(projectId, accessToken) shouldBe error

      logger.loggedOnly(Error(s"Hook validation fails for project with id $projectId", exception))
    }

    "log an error if finding project hook url fails" in new TestCase {

      val projectInfo = projectInfos.generateOne
      val projectId   = projectInfo.id

      (projectInfoFinder
        .findProjectInfo(_: ProjectId, _: AccessToken))
        .expects(projectId, accessToken)
        .returning(context.pure(projectInfo))

      val exception: Exception    = exceptions.generateOne
      val error:     Try[Nothing] = context.raiseError(exception)
      (projectHookUrlFinder.findProjectHookUrl _)
        .expects()
        .returning(error)

      validator.validateHook(projectId, accessToken) shouldBe error

      logger.loggedOnly(Error(s"Hook validation fails for project with id $projectId", exception))
    }

    "log an error if finding hook verification fails" in new TestCase {

      val projectInfo = projectInfos.generateOne
      val projectId   = projectInfo.id

      (projectInfoFinder
        .findProjectInfo(_: ProjectId, _: AccessToken))
        .expects(projectId, accessToken)
        .returning(context.pure(projectInfo))

      val projectHookUrl = projectHookUrls.generateOne
      (projectHookUrlFinder.findProjectHookUrl _)
        .expects()
        .returning(context.pure(projectHookUrl))

      val exception: Exception    = exceptions.generateOne
      val error:     Try[Nothing] = context.raiseError(exception)
      (projectHookVerifier
        .checkProjectHookPresence(_: HookIdentifier, _: AccessToken))
        .expects(HookIdentifier(projectId, projectHookUrl), accessToken)
        .returning(error)

      validator.validateHook(projectId, accessToken) shouldBe error

      logger.loggedOnly(Error(s"Hook validation fails for project with id $projectId", exception))
    }
  }

  private trait TestCase {
    val context = MonadError[Try, Throwable]

    val accessToken = accessTokens.generateOne

    val projectInfoFinder    = mock[ProjectInfoFinder[Try]]
    val projectHookUrlFinder = mock[TryProjectHookUrlFinder]
    val projectHookVerifier  = mock[ProjectHookVerifier[Try]]
    val logger               = TestLogger[Try]()
    val validator            = new HookValidator[Try](projectInfoFinder, projectHookUrlFinder, projectHookVerifier, logger)
  }
}

class TryHookValidator(
    projectInfoFinder:    ProjectInfoFinder[Try],
    projectHookUrlFinder: ProjectHookUrlFinder[Try],
    projectHookVerifier:  ProjectHookVerifier[Try],
    logger:               Logger[Try]
) extends HookValidator[Try](projectInfoFinder, projectHookUrlFinder, projectHookVerifier, logger)