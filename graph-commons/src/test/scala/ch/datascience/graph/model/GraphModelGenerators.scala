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

package ch.datascience.graph.model

import ch.datascience.generators.Generators._
import ch.datascience.graph.model.datasets._
import ch.datascience.graph.model.projects.{FilePath, FullProjectPath, ProjectPath}
import org.scalacheck.Gen
import org.scalacheck.Gen.uuid

object GraphModelGenerators {

  implicit val projectPaths: Gen[ProjectPath] = relativePaths(minSegments = 2, maxSegments = 2) map ProjectPath.apply
  implicit val fullProjectPaths: Gen[FullProjectPath] = for {
    url  <- httpUrls
    path <- projectPaths
  } yield FullProjectPath.from(s"$url/$path").fold(throw _, identity)
  implicit val filePaths: Gen[FilePath] = relativePaths() map FilePath.apply

  implicit val datasetIds: Gen[Identifier] = Gen
    .oneOf(
      uuid.map(_.toString),
      for {
        first  <- Gen.choose(10, 99)
        second <- Gen.choose(1000, 9999)
        third  <- Gen.choose(1000000, 9999999)
      } yield s"$first.$second/zenodo.$third"
    )
    .map(Identifier.apply)
  implicit val datasetNames:          Gen[Name]          = nonEmptyStrings() map Name.apply
  implicit val datasetDescriptions:   Gen[Description]   = nonEmptyStrings(maxLength = 1000) map Description.apply
  implicit val datasetPublishedDates: Gen[PublishedDate] = localDatesNotInTheFuture map PublishedDate.apply
  implicit val datasetPartNames:      Gen[PartName]      = nonEmptyStrings() map PartName.apply
  implicit val datasetPartLocations: Gen[PartLocation] =
    relativePaths(minSegments = 2, maxSegments = 2)
      .map(path => s"data/$path")
      .map(PartLocation.apply)
  implicit val datasetInProjectCreationDates: Gen[DateCreatedInProject] =
    timestampsNotInTheFuture map DateCreatedInProject.apply
}