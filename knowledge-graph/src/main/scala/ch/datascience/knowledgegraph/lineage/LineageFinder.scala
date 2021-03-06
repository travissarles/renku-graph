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

package ch.datascience.knowledgegraph.lineage

import cats.effect._
import cats.implicits._
import ch.datascience.graph.config.RenkuBaseUrl
import ch.datascience.graph.model.projects.{FilePath, Path, ResourceId}
import ch.datascience.graph.model.views.RdfResource
import ch.datascience.logging.ApplicationLogger
import ch.datascience.rdfstore._
import eu.timepit.refined.auto._
import io.chrisdavenport.log4cats.Logger
import model._

import scala.concurrent.ExecutionContext
import scala.language.higherKinds

trait LineageFinder[Interpretation[_]] {
  def findLineage(projectPath: Path, filePath: FilePath): Interpretation[Option[Lineage]]
}

class IOLineageFinder(
    rdfStoreConfig:          RdfStoreConfig,
    renkuBaseUrl:            RenkuBaseUrl,
    logger:                  Logger[IO],
    timeRecorder:            SparqlQueryTimeRecorder[IO]
)(implicit executionContext: ExecutionContext, contextShift: ContextShift[IO], timer: Timer[IO])
    extends IORdfStoreClient(rdfStoreConfig, logger, timeRecorder)
    with LineageFinder[IO] {

  private type EdgeData = (Node.Id, Node.Location, Node.Id, Node.Location)

  override def findLineage(projectPath: Path, filePath: FilePath): IO[Option[Lineage]] =
    for {
      edgesAndLocations <- queryExpecting[Set[EdgeData]](using = query(projectPath, filePath))
      nodes <- edgesAndLocations.toNodesIdsSet.toList
                .map(toNodeQuery(projectPath))
                .map(queryExpecting[Option[Node]](_).flatMap(toNodeOrError(projectPath)))
                .parSequence
                .map(_.toSet)
      maybeLineage <- toLineage(edgesAndLocations, nodes)
    } yield maybeLineage

  private def query(path: Path, filePath: FilePath) = SparqlQuery(
    name = "lineage",
    Set(
      "PREFIX prov: <http://www.w3.org/ns/prov#>",
      "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>",
      "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>",
      "PREFIX wfprov: <http://purl.org/wf4ever/wfprov#>",
      "PREFIX schema: <http://schema.org/>"
    ),
    s"""|SELECT ?sourceId ?sourceLocation ?targetId ?targetLocation
        |WHERE {
        |  {
        |    SELECT (MIN(?startedAt) AS ?minStartedAt)
        |    WHERE {
        |      ?qentity schema:isPartOf ${ResourceId(renkuBaseUrl, path).showAs[RdfResource]};
        |               prov:atLocation ?location.
        |      FILTER (strStarts("$filePath", ?location))
        |      ?qentity (prov:qualifiedGeneration/prov:activity | ^prov:entity/^prov:qualifiedUsage) ?activityId.
        |      ?activityId rdf:type <http://www.w3.org/ns/prov#Activity>;
        |                  prov:startedAtTime ?startedAt.
        |    }
        |  } {
        |    SELECT ?entity
        |    WHERE {
        |      ?qentity schema:isPartOf ${ResourceId(renkuBaseUrl, path).showAs[RdfResource]};
        |               prov:atLocation ?location.
        |      FILTER (strStarts("$filePath", ?location))
        |      ?qentity (prov:qualifiedGeneration/prov:activity | ^prov:entity/^prov:qualifiedUsage) ?activityId.
        |      ?activityId rdf:type <http://www.w3.org/ns/prov#Activity>;
        |                  prov:startedAtTime ?minStartedAt.
        |      ?qentity (
        |        ^(prov:qualifiedGeneration/prov:activity/prov:qualifiedUsage/prov:entity)* | (prov:qualifiedGeneration/prov:activity/prov:qualifiedUsage/prov:entity)*
        |      ) ?entity .
        |    }
        |    GROUP BY ?entity
        |  } {
        |    ?entity prov:qualifiedGeneration/prov:activity ?activity.
        |    FILTER NOT EXISTS {?entity rdfs:comment "renku update"}
        |    FILTER NOT EXISTS {?activity rdfs:comment "renku update"}
        |    FILTER NOT EXISTS {?activity rdf:type wfprov:WorkflowRun}
        |    FILTER EXISTS {?activity rdf:type wfprov:ProcessRun}
        |    BIND (?entity AS ?targetId)
        |    BIND (?activity AS ?sourceId)
        |    ?sourceId prov:atLocation ?sourceLocation.
        |    ?targetId prov:atLocation ?targetLocation.
        |  } UNION {
        |    ?activity prov:qualifiedUsage/prov:entity ?entity.
        |    FILTER NOT EXISTS {?entity rdfs:comment "renku update"}
        |    FILTER NOT EXISTS {?activity rdfs:comment "renku update"}
        |    FILTER NOT EXISTS {?activity rdf:type wfprov:WorkflowRun}
        |    FILTER EXISTS {?activity rdf:type wfprov:ProcessRun}
        |    BIND (?activity AS ?targetId)
        |    BIND (?entity AS ?sourceId)
        |    ?sourceId prov:atLocation ?sourceLocation.
        |    ?targetId prov:atLocation ?targetLocation.
        |  }
        |}
        |""".stripMargin
  )

  private def toNodeQuery(path: Path)(nodeId: Node.Id) = SparqlQuery(
    name = "lineage - node details",
    Set(
      "PREFIX prov: <http://www.w3.org/ns/prov#>",
      "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>",
      "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>"
    ),
    s"""|SELECT ?type ?location ?label ?maybeComment
        |WHERE {
        |  {
        |    ${nodeId.showAs[RdfResource]} rdf:type ?type;
        |                                  rdfs:label ?label;
        |                                  prov:atLocation ?location.
        |    OPTIONAL { ${nodeId.showAs[RdfResource]} rdfs:comment ?maybeComment }
        |  }
        |}
        |""".stripMargin
  )

  import ch.datascience.tinytypes.json.TinyTypeDecoders
  import io.circe.Decoder

  private implicit val nodeIdDecoder:   Decoder[Node.Id]       = TinyTypeDecoders.stringDecoder(Node.Id)
  private implicit val locationDecoder: Decoder[Node.Location] = TinyTypeDecoders.stringDecoder(Node.Location)

  private implicit val edgesDecoder: Decoder[Set[EdgeData]] = {
    implicit lazy val edgeDecoder: Decoder[EdgeData] = { implicit cursor =>
      for {
        sourceId       <- cursor.downField("sourceId").downField("value").as[Node.Id]
        sourceLocation <- cursor.downField("sourceLocation").downField("value").as[Node.Location]
        targetId       <- cursor.downField("targetId").downField("value").as[Node.Id]
        targetLocation <- cursor.downField("targetLocation").downField("value").as[Node.Location]
      } yield (sourceId, sourceLocation, targetId, targetLocation)
    }

    _.downField("results").downField("bindings").as[List[(Node.Id, Node.Location, Node.Id, Node.Location)]].map(_.toSet)
  }

  private implicit val nodeDecoder: Decoder[Option[Node]] = {
    implicit val labelDecoder: Decoder[Node.Label] = TinyTypeDecoders.stringDecoder(Node.Label)
    implicit val typeDecoder:  Decoder[Node.Type]  = TinyTypeDecoders.stringDecoder(Node.Type)

    implicit lazy val fieldsDecoder: Decoder[(Node.Location, Node.Type, Node.Label)] = { implicit cursor =>
      for {
        nodeType     <- cursor.downField("type").downField("value").as[Node.Type]
        location     <- cursor.downField("location").downField("value").as[Node.Location]
        label        <- cursor.downField("label").downField("value").as[Node.Label]
        maybeComment <- cursor.downField("maybeComment").downField("value").as[Option[Node.Label]]
      } yield (location, nodeType, maybeComment getOrElse label)
    }

    lazy val maybeToNode: List[(Node.Location, Node.Type, Node.Label)] => Option[Node] = {
      case Nil => None
      case (location, typ, label) +: tail =>
        Some {
          tail.foldLeft(Node(location, label, Set(typ))) {
            case (node, (`location`, t, `label`)) => node.copy(types = node.types + t)
          }
        }
    }

    _.downField("results")
      .downField("bindings")
      .as[List[(Node.Location, Node.Type, Node.Label)]]
      .map(maybeToNode)
  }

  private implicit class EdgesAndLocationsOps(edges: Set[EdgeData]) {
    lazy val toNodesIdsSet: Set[Node.Id] = edges.foldLeft(Set.empty[Node.Id]) {
      case (acc, (leftEdge, _, rightEdge, _)) => acc + leftEdge + rightEdge
    }
  }

  private def toNodeOrError(projectPath: Path): Option[Node] => IO[Node] = {
    case Some(node) => node.pure[IO]
    case _          => new Exception(s"Cannot find node details for $projectPath").raiseError[IO, Node]
  }

  private lazy val toLineage: (Set[EdgeData], Set[Node]) => IO[Option[Lineage]] = {
    case (edgesAndLocations, _) if edgesAndLocations.isEmpty => IO.pure(Option.empty)
    case (edgesAndLocations, nodes)                          => Lineage.from[IO](edgesAndLocations.toEdges, nodes) map Option.apply
  }

  private implicit class EdgeDataOps(edgesAndLocations: Set[EdgeData]) {
    lazy val toEdges: Set[Edge] = edgesAndLocations map {
      case (_, sourceLocation, _, targetLocation) => Edge(sourceLocation, targetLocation)
    }
  }
}

object IOLineageFinder {

  def apply(
      timeRecorder:            SparqlQueryTimeRecorder[IO],
      rdfStoreConfig:          IO[RdfStoreConfig] = RdfStoreConfig[IO](),
      renkuBaseUrl:            IO[RenkuBaseUrl] = RenkuBaseUrl[IO](),
      logger:                  Logger[IO] = ApplicationLogger
  )(implicit executionContext: ExecutionContext,
    contextShift:              ContextShift[IO],
    timer:                     Timer[IO]): IO[LineageFinder[IO]] =
    for {
      config       <- rdfStoreConfig
      renkuBaseUrl <- renkuBaseUrl
    } yield new IOLineageFinder(
      config,
      renkuBaseUrl,
      logger,
      timeRecorder
    )
}
