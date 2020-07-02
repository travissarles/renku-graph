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
package datasets

import cats.MonadError
import cats.data.EitherT
import ch.datascience.graph.model.datasets.{DerivedFrom, SameAs}
import ch.datascience.triplesgenerator.eventprocessing.CommitEventProcessor.ProcessingRecoverableError

import scala.language.higherKinds
import cats.implicits._

private[triplescuration] class DataSetInfoEnricher[Interpretation[_]](
    dataSetInfoFinder: DataSetInfoFinder[Interpretation],
    updatesCreator:    UpdatesCreator,
    topmostDataFinder: TopmostDataFinder[Interpretation]
)(implicit ME:         MonadError[Interpretation, Throwable]) {

  def enrichDataSetInfo(curatedTriples: CuratedTriples): CurationResults[Interpretation] =
    EitherT.right {
      for {
        datasetInfos <- dataSetInfoFinder.findDatasetsInfo(curatedTriples.triples)
        topmostInfos = datasetInfos.map(topmostDataFinder.findTopmostData)
      } yield topmostInfos.foldLeft(curatedTriples) {
        case (curated, topmostInfo) =>
          curated.addUpdates(updatesCreator.prepareUpdates(topmostInfo))
      }
    }
}