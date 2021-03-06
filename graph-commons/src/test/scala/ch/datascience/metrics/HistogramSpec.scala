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

package ch.datascience.metrics

import cats.MonadError
import cats.implicits._
import ch.datascience.generators.Generators.Implicits._
import ch.datascience.generators.Generators._
import io.prometheus.client.{Histogram => LibHistogram}
import org.scalamock.scalatest.MockFactory
import org.scalatest.Matchers._
import org.scalatest.WordSpec

import scala.util.{Success, Try}

class HistogramSpec extends WordSpec with MockFactory {

  "apply" should {

    "register the metrics in the Metrics Registry " +
      "and return an instance of the LabeledHistogram" in new TestCase {

      (metricsRegistry
        .register[LibHistogram, LibHistogram.Builder](_: LibHistogram.Builder)(_: MonadError[Try, Throwable]))
        .expects(*, *)
        .onCall { (builder: LibHistogram.Builder, _: MonadError[Try, Throwable]) =>
          builder.create().pure[Try]
        }

      val labelName = nonBlankStrings().generateOne

      val Success(histogram) = Histogram[Try, String](name, help, labelName, Seq(.1, 1))(metricsRegistry)

      histogram.isInstanceOf[LabeledHistogram[Try, String]] shouldBe true
      histogram.name                                        shouldBe name.value
      histogram.help                                        shouldBe help.value
    }
  }

  private trait TestCase {
    val ME = MonadError[Try, Throwable]

    val name = nonBlankStrings().generateOne
    val help = sentences().generateOne

    val metricsRegistry = mock[MetricsRegistry[Try]]
  }
}

class LabeledHistogramSpec extends WordSpec with MockFactory {

  import MetricsTools._
  import ch.datascience.graph.model.GraphModelGenerators._
  import ch.datascience.graph.model.projects.Path

  "startTimer -> observeDuration" should {

    "associate measured time with the label on the histogram" in new TestCase {

      // iteration 1
      val labelValue1    = projectPaths.generateOne
      val Success(timer) = histogram.startTimer(labelValue1)

      val sleepTime = 500
      Thread sleep sleepTime

      val Success(duration) = timer.observeDuration

      (duration * 1000) should be > sleepTime.toDouble

      underlying.collectAllSamples.map(_._1)             should contain(label)
      underlying.collectAllSamples.map(_._2)             should contain(labelValue1.toString)
      underlying.collectAllSamples.map(_._3).last * 1000 should be > sleepTime.toDouble
    }
  }

  private trait TestCase {
    val ME = MonadError[Try, Throwable]

    val label        = nonBlankStrings().generateOne.value
    private val name = nonBlankStrings().generateOne
    private val help = sentences().generateOne
    val underlying   = LibHistogram.build(name.value, help.value).labelNames(label).create()

    val resetDataFetch = mockFunction[Try[Map[Path, Double]]]
    val histogram      = new LabeledHistogramImpl[Try, Path](underlying)
  }
}
