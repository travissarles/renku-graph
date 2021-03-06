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

package ch.datascience.control

import cats.MonadError
import ch.datascience.config.ConfigLoader
import ch.datascience.tinytypes.TypeName
import com.typesafe.config.{Config, ConfigFactory}
import eu.timepit.refined.api.{RefType, Refined}
import eu.timepit.refined.numeric.Positive

import scala.concurrent.duration._
import scala.language.{higherKinds, postfixOps}
import scala.util.Try

case class RateLimit[Target](items: Long Refined Positive, per: RateLimitUnit) {
  import RateLimitUnit._

  override lazy val toString: String = per match {
    case Second => s"$items/sec"
    case Minute => s"$items/min"
    case Hour   => s"$items/hour"
    case Day    => s"$items/day"
  }
}

sealed trait RateLimitUnit extends Product with Serializable
object RateLimitUnit extends TypeName {
  case object Second extends RateLimitUnit
  case object Minute extends RateLimitUnit
  case object Hour   extends RateLimitUnit
  case object Day    extends RateLimitUnit

  def from[Interpretation[_]](
      value:     String
  )(implicit ME: MonadError[Interpretation, Throwable]): Interpretation[RateLimitUnit] = value match {
    case "sec"  => ME.pure(Second)
    case "min"  => ME.pure(Minute)
    case "hour" => ME.pure(Hour)
    case "day"  => ME.pure(Day)
    case other  => ME.raiseError(new IllegalArgumentException(s"Unknown '$other' for $typeName"))
  }

  implicit class RateLimitUnitOps(unit: RateLimitUnit) {

    def multiplierFor(newUnit: TimeUnit): Double =
      finiteDurationFrom(unit) toUnit newUnit

    private lazy val finiteDurationFrom: RateLimitUnit => FiniteDuration = {
      case Second => 1 second
      case Minute => 1 minute
      case Hour   => 1 hour
      case Day    => 1 day
    }
  }
}

object RateLimit extends TypeName {
  import cats.implicits._

  private val RateExtractor = """(\d+)[ ]*/(\w+)""".r

  def from[Interpretation[_], Target](
      value:     String
  )(implicit ME: MonadError[Interpretation, Throwable]): Interpretation[RateLimit[Target]] = value match {
    case RateExtractor(rate, unit) =>
      (toPositiveLong[Interpretation](rate), RateLimitUnit.from[Interpretation](unit)) mapN RateLimit[Target]
    case other => ME.raiseError(new IllegalArgumentException(s"Invalid value for $typeName: '$other'"))
  }

  def fromConfig[Interpretation[_], Target](
      key:       String,
      config:    Config = ConfigFactory.load()
  )(implicit ME: MonadError[Interpretation, Throwable]): Interpretation[RateLimit[Target]] = {
    import ConfigLoader._
    import pureconfig.ConfigReader
    import pureconfig.error.CannotConvert

    implicit val rateLimitReader: ConfigReader[RateLimit[Target]] =
      ConfigReader.fromString[RateLimit[Target]] { value =>
        RateLimit
          .from[Try, Target](value)
          .toEither
          .leftMap(exception => CannotConvert(value, RateLimit.getClass.toString, exception.getMessage))
      }

    find[Interpretation, RateLimit[Target]](key, config)
  }

  private def toPositiveLong[Interpretation[_]](
      rate:      String
  )(implicit ME: MonadError[Interpretation, Throwable]): Interpretation[Long Refined Positive] =
    for {
      long <- ME
               .fromTry(Try(rate.toLong))
               .adaptError { case _ => new IllegalArgumentException(s"$typeName has to be positive") }
      positiveLong <- ME.fromEither(long.toPositiveLong(errorWhenNotPositive = s"$typeName has to be positive"))
    } yield positiveLong

  implicit class RateLimitOps[OldTarget](rateLimit: RateLimit[OldTarget]) {
    import RateLimitUnit._

    def /[NewTarget](divider: Int Refined Positive): Either[IllegalArgumentException, RateLimit[NewTarget]] =
      (rateLimit.items.value * (1 day).toMillis / (rateLimit.per.multiplierFor(MILLISECONDS) * divider.value)).toLong
        .toPositiveLong("RateLimits below 1/day not supported")
        .map(RateLimit[NewTarget](_, per = Day))
  }

  private implicit class LongOps(value: Long) {
    def toPositiveLong(errorWhenNotPositive: String): Either[IllegalArgumentException, Long Refined Positive] =
      RefType
        .applyRef[Long Refined Positive](value)
        .leftMap(_ => new IllegalArgumentException(errorWhenNotPositive))
  }
}
