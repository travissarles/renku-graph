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

import ch.datascience.metrics.MetricsRegistry.{DisabledMetricsRegistry, EnabledMetricsRegistry}
import com.typesafe.config.ConfigFactory
import io.prometheus.client.{Gauge => LibGauge}
import org.scalatest.Matchers._
import org.scalatest.WordSpec

import scala.collection.JavaConverters._

class MetricsRegistrySpec extends WordSpec {

  "apply" should {

    "return a disabled Metrics Registry if the 'metrics.enabled' flag is set to false" in {
      val registry = MetricsRegistry(
        ConfigFactory.parseMap(Map("metrics" -> Map("enabled" -> false).asJava).asJava)
      ).unsafeRunSync()

      registry shouldBe a[DisabledMetricsRegistry.type]
    }

    "return an enabled Metrics Registry if the 'metrics.enabled' flag is set to true" in {
      val registry = MetricsRegistry(
        ConfigFactory.parseMap(Map("metrics" -> Map("enabled" -> true).asJava).asJava)
      ).unsafeRunSync()

      registry shouldBe a[EnabledMetricsRegistry.type]
    }

    "return an enabled Metrics Registry if there is no value for the 'metrics.enabled' flag" in {
      MetricsRegistry(ConfigFactory.empty()).unsafeRunSync() shouldBe a[EnabledMetricsRegistry.type]
    }
  }

  "EnabledMetricsRegistry.register" should {

    "register the given collector in the collector registry" in {
      val gaugeName = "gauge_name"

      val gauge = EnabledMetricsRegistry
        .register[LibGauge, LibGauge.Builder](
          LibGauge
            .build()
            .name(gaugeName)
            .help("some gauge info")
            .labelNames("label")
        )
        .unsafeRunSync()

      gauge.labels("lbl").set(2)

      EnabledMetricsRegistry.maybeCollectorRegistry.flatMap(
        _.metricFamilySamples().asScala
          .find(_.name == gaugeName)
          .map(_.samples.asScala.map(_.value).toList)
      ) shouldBe Some(List(2))
    }
  }

  "DisabledMetricsRegistry.register" should {

    "not register the given collector in any registry" in {
      val gaugeName = "gauge_name"

      val gauge = DisabledMetricsRegistry
        .register[LibGauge, LibGauge.Builder](
          LibGauge
            .build()
            .name(gaugeName)
            .help("some gauge info")
            .labelNames("label")
        )
        .unsafeRunSync()

      gauge.labels("lbl").set(2)

      DisabledMetricsRegistry.maybeCollectorRegistry shouldBe None
    }
  }
}
