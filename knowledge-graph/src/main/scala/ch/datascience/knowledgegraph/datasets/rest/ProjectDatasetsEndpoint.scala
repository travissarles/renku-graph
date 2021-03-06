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

package ch.datascience.knowledgegraph.datasets.rest

import cats.effect._
import cats.implicits._
import ch.datascience.controllers.ErrorMessage
import ch.datascience.controllers.InfoMessage._
import ch.datascience.graph.model.datasets.{Identifier, Name, SameAs}
import ch.datascience.http.rest.Links
import Links._
import ch.datascience.config.renku
import ch.datascience.graph.config.RenkuBaseUrl
import ch.datascience.graph.model.projects
import ch.datascience.logging.{ApplicationLogger, ExecutionTimeRecorder}
import ch.datascience.rdfstore.{RdfStoreConfig, SparqlQueryTimeRecorder}
import io.chrisdavenport.log4cats.Logger
import io.circe.Encoder
import io.circe.literal._
import io.circe.syntax._
import org.http4s.Response
import org.http4s.dsl.Http4sDsl

import scala.concurrent.ExecutionContext
import scala.language.higherKinds
import scala.util.control.NonFatal

class ProjectDatasetsEndpoint[Interpretation[_]: Effect](
    projectDatasetsFinder: ProjectDatasetsFinder[Interpretation],
    renkuResourcesUrl:     renku.ResourcesUrl,
    executionTimeRecorder: ExecutionTimeRecorder[Interpretation],
    logger:                Logger[Interpretation]
) extends Http4sDsl[Interpretation] {

  import executionTimeRecorder._
  import org.http4s.circe._

  def getProjectDatasets(projectPath: projects.Path): Interpretation[Response[Interpretation]] =
    measureExecutionTime {
      projectDatasetsFinder
        .findProjectDatasets(projectPath)
        .flatMap(datasets => Ok(datasets.asJson))
        .recoverWith(httpResult(projectPath))
    } map logExecutionTimeWhen(finishedSuccessfully(projectPath))

  private def httpResult(
      projectPath: projects.Path
  ): PartialFunction[Throwable, Interpretation[Response[Interpretation]]] = {
    case NonFatal(exception) =>
      val errorMessage = ErrorMessage(s"Finding $projectPath's datasets failed")
      logger.error(exception)(errorMessage.value)
      InternalServerError(errorMessage)
  }

  private def finishedSuccessfully(projectPath: projects.Path): PartialFunction[Response[Interpretation], String] = {
    case response if response.status == Ok => s"Finding '$projectPath' datasets finished"
  }

  private implicit val datasetEncoder: Encoder[(Identifier, Name, SameAs)] =
    Encoder.instance[(Identifier, Name, SameAs)] {
      case (id, name, sameAs) =>
        json"""{
          "identifier": ${id.toString},
          "name": ${name.toString},
          "sameAs": ${sameAs.toString}
        }""" deepMerge _links(
          Link(Rel("details") -> Href(renkuResourcesUrl / "datasets" / id))
        )
    }
}

object IOProjectDatasetsEndpoint {

  def apply(
      timeRecorder:            SparqlQueryTimeRecorder[IO]
  )(implicit executionContext: ExecutionContext,
    contextShift:              ContextShift[IO],
    timer:                     Timer[IO]): IO[ProjectDatasetsEndpoint[IO]] =
    for {
      rdfStoreConfig        <- RdfStoreConfig[IO]()
      renkuBaseUrl          <- RenkuBaseUrl[IO]()
      renkuResourceUrl      <- renku.ResourcesUrl[IO]()
      executionTimeRecorder <- ExecutionTimeRecorder[IO](ApplicationLogger)
    } yield new ProjectDatasetsEndpoint[IO](
      new IOProjectDatasetsFinder(rdfStoreConfig, renkuBaseUrl, ApplicationLogger, timeRecorder),
      renkuResourceUrl,
      executionTimeRecorder,
      ApplicationLogger
    )
}
