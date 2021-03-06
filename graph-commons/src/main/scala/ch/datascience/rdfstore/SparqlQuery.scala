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

package ch.datascience.rdfstore

import ch.datascience.http.rest.paging.PagingRequest
import eu.timepit.refined.api.Refined
import eu.timepit.refined.collection.NonEmpty

import scala.language.higherKinds

final case class SparqlQuery(name:               String Refined NonEmpty,
                             prefixes:           Set[String Refined NonEmpty],
                             body:               String,
                             maybePagingRequest: Option[PagingRequest] = None) {
  override lazy val toString: String =
    s"""|${prefixes.mkString("", "\n", "")}
        |$body
        |$pagingRequest""".stripMargin.trim

  private lazy val pagingRequest =
    maybePagingRequest
      .map { pagingRequest =>
        import pagingRequest._
        s"""|LIMIT $perPage
            |OFFSET ${(page.value - 1) * perPage.value}""".stripMargin
      }
      .getOrElse("")
}

object SparqlQuery {

  import cats.MonadError
  import cats.implicits._

  val totalField: String = "total"

  implicit class SparqlQueryOps(sparqlQuery: SparqlQuery) {

    def include[Interpretation[_]](
        pagingRequest: PagingRequest
    )(implicit ME:     MonadError[Interpretation, Throwable]): Interpretation[SparqlQuery] =
      if (sparqlQuery.body.trim.matches("(?si)^.*(ORDER[ ]+BY[ ]+(ASC|DESC)[ ]*\\([ ]*\\?\\w+[ ]*\\))$"))
        sparqlQuery.copy(maybePagingRequest = Some(pagingRequest)).pure[Interpretation]
      else
        new Exception("Sparql query cannot be used for paging as there's no ending ORDER BY clause")
          .raiseError[Interpretation, SparqlQuery]

    lazy val toCountQuery: SparqlQuery = sparqlQuery.copy(
      body = s"""|SELECT (COUNT(*) AS ?$totalField)
                 |WHERE {
                 |  ${sparqlQuery.body}
                 |}""".stripMargin
    )
  }
}
