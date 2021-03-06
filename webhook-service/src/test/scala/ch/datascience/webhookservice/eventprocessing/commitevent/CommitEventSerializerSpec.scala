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

package ch.datascience.webhookservice.eventprocessing.commitevent

import cats.MonadError
import cats.implicits._
import ch.datascience.generators.Generators.Implicits._
import ch.datascience.webhookservice.eventprocessing.{CommitEvent, Person}
import ch.datascience.webhookservice.generators.WebhookServiceGenerators._
import io.circe.Json
import io.circe.parser._
import org.scalatest.Matchers._
import org.scalatest.WordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.util.Try

class CommitEventSerializerSpec extends WordSpec with ScalaCheckPropertyChecks {

  "serialiseToJsonString" should {

    "return a single line json if serialization was successful" in new TestCase {
      forAll { commitEvent: CommitEvent =>
        serializer.serialiseToJsonString(commitEvent).map(parse) shouldBe json(
          "id"            -> Json.fromString(commitEvent.id.value),
          "message"       -> Json.fromString(commitEvent.message.value),
          "committedDate" -> Json.fromString(commitEvent.committedDate.toString),
          "author"        -> commitEvent.author.toJson,
          "committer"     -> commitEvent.committer.toJson,
          "parents"       -> Json.fromValues(commitEvent.parents.map(parent => Json.fromString(parent.value))),
          "project" -> Json.obj(
            "id"   -> Json.fromInt(commitEvent.project.id.value),
            "path" -> Json.fromString(commitEvent.project.path.value)
          )
        )
      }
    }
  }

  private trait TestCase {
    val context = MonadError[Try, Throwable]

    val serializer = new CommitEventSerializer[Try]()

    def json(fields: (String, Json)*) = context.pure {
      Right(Json.obj(fields: _*))
    }
  }

  private implicit class PersonOps(person: Person) {
    lazy val toJson: Json = person match {
      case person: Person.WithEmail =>
        Json.obj(
          "username" -> Json.fromString(person.name.value),
          "email"    -> Json.fromString(person.email.value)
        )
      case person: Person => Json.obj("username" -> Json.fromString(person.name.value))
      case _ => Json.Null
    }
  }
}
