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

package ch.datascience.knowledgegraph.graphql

import cats.effect.IO
import ch.datascience.controllers.ErrorMessage
import ch.datascience.controllers.ErrorMessage._
import ch.datascience.generators.Generators.Implicits._
import ch.datascience.generators.Generators._
import ch.datascience.http.server.EndpointTester._
import io.circe.Json
import io.circe.literal._
import io.circe.syntax._
import org.http4s.MediaType._
import org.http4s.Status._
import org.http4s._
import org.http4s.headers.`Content-Type`
import org.scalamock.matchers.MatcherBase
import org.scalamock.scalatest.MockFactory
import org.scalatest.Matchers._
import org.scalatest.WordSpec
import sangria.execution.{ExceptionHandler, QueryAnalysisError}
import sangria.schema._

class QueryEndpointSpec extends WordSpec with MockFactory {

  "handleQuery" should {

    "respond with OK and the results found by the Query Runner when there are no variables in the query" in new TestCase {

      val request = Request[IO](Method.POST, uri"graphql").withEntity(queryWithNoVariablesPayload)

      val queryResult = jsons.generateOne
      (queryRunner
        .run(_: UserQuery))
        .expects(queryMatchingRequest)
        .returning(IO.pure(queryResult))

      val response = handleQuery(request).unsafeRunSync()

      response.status                   shouldBe Ok
      response.contentType              shouldBe Some(`Content-Type`(application.json))
      response.as[Json].unsafeRunSync() shouldBe queryResult
    }

    "respond with OK and the results found by the Query Runner when there are variables in the query" in new TestCase {

      val request = Request[IO](Method.POST, uri"graphql").withEntity(queryWithVariablesPayload)

      val queryResult = jsons.generateOne
      (queryRunner
        .run(_: UserQuery))
        .expects(queryAndVariablesMatchingRequest)
        .returning(IO.pure(queryResult))

      val response = handleQuery(request).unsafeRunSync()

      response.status                   shouldBe Ok
      response.contentType              shouldBe Some(`Content-Type`(application.json))
      response.as[Json].unsafeRunSync() shouldBe queryResult
    }

    "respond with BAD_REQUEST if the given query is invalid" in new TestCase {

      val request = Request[IO](Method.POST, uri"graphql").withEntity("")

      val response = handleQuery(request).unsafeRunSync()

      response.status                                 shouldBe BadRequest
      response.contentType                            shouldBe Some(`Content-Type`(application.json))
      response.as[ErrorMessage].unsafeRunSync().value should include("Invalid JSON")
    }

    "respond with BAD_REQUEST if running the query fails with QueryAnalysisError" in new TestCase {

      val request = Request[IO](Method.POST, uri"graphql").withEntity(queryWithNoVariablesPayload)

      val exception = queryAnalysisErrors.generateOne
      (queryRunner
        .run(_: UserQuery))
        .expects(queryMatchingRequest)
        .returning(IO.raiseError(exception))

      val response = handleQuery(request).unsafeRunSync()

      response.status                   shouldBe BadRequest
      response.contentType              shouldBe Some(`Content-Type`(application.json))
      response.as[Json].unsafeRunSync() shouldBe ErrorMessage(exception).asJson
    }

    "respond with INTERNAL_SERVER_ERROR if running the query fails" in new TestCase {

      val request = Request[IO](Method.POST, uri"graphql").withEntity(queryWithNoVariablesPayload)

      val exception = exceptions.generateOne
      (queryRunner
        .run(_: UserQuery))
        .expects(queryMatchingRequest)
        .returning(IO.raiseError(exception))

      val response = handleQuery(request).unsafeRunSync()

      response.status                   shouldBe InternalServerError
      response.contentType              shouldBe Some(`Content-Type`(application.json))
      response.as[Json].unsafeRunSync() shouldBe ErrorMessage(exception).asJson
    }
  }

  private trait TestCase {
    private val querySchema: Schema[QueryContext[IO], Unit] = Schema(
      ObjectType("test", fields[QueryContext[IO], Unit](Field("field", StringType, None, Nil, _ => "value")))
    )
    val queryRunner = mock[IOQueryRunner]
    val handleQuery = new QueryEndpoint[IO](querySchema, queryRunner).handleQuery _

    private val queryWithNoVariables     = """{ resource { property } }"""
    lazy val queryWithNoVariablesPayload = json"""
      {                                                      
        "query": $queryWithNoVariables
      }"""
    val queryMatchingRequest: MatcherBase = argAssert { userQuery: UserQuery =>
      userQuery.query.source shouldBe Some(queryWithNoVariables)
      ()
    }

    private val queryWithVariables     = """query($variable: Type!) { resource(variable: $variable) { property } }"""
    lazy val queryWithVariablesPayload = json"""
      {
        "query": $queryWithVariables,
        "variables": {"variable": "value"}
      }"""
    val queryAndVariablesMatchingRequest: MatcherBase = argAssert { userQuery: UserQuery =>
      userQuery.query.source shouldBe Some(queryWithVariables)
      userQuery.variables    shouldBe Map("variable" -> "value")
      ()
    }
  }

  private val queryAnalysisErrors =
    nonEmptyStrings()
      .map { message =>
        new Exception(message) with QueryAnalysisError { override def exceptionHandler = ExceptionHandler.empty }
      }
}
