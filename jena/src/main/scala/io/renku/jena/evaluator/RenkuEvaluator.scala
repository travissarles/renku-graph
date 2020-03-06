package io.renku.jena.evaluator

import java.util

import io.renku.jena.realm.PrivilegedUsers
import org.apache.jena.graph
import org.apache.jena.graph.{Node, Triple}
import org.apache.jena.permissions.SecurityEvaluator
import org.apache.jena.rdf.model._
import org.apache.jena.shared.AuthenticationRequiredException
import org.apache.jena.vocabulary.RDF
import org.apache.shiro.SecurityUtils
import org.apache.shiro.subject.Subject

import scala.collection.JavaConverters._

class RenkuEvaluator(smth: Any) extends SecurityEvaluator {

  private val projectIdPrefix: String = "https://dev.renku.ch/projects/"
  private val isPartOf = ResourceFactory.createProperty("http://schema.org/", "isPartOf")

  override def evaluate(principal: Any, action: SecurityEvaluator.Action, graphIRI: Node): Boolean = {
    val r = principal.isAuthenticated && (
      (action == SecurityEvaluator.Action.Read) || principal.isPrivileged
    )

    println(s"evaluate 1 $action -> $r")
    r
  }

  override def evaluate(principal: Any,
                        action:    SecurityEvaluator.Action,
                        graphIRI:  Node,
                        triple:    graph.Triple): Boolean = {
    println("evaluate 2")
    evaluate(principal, action, graphIRI) && (principal.isPrivileged || evaluate(principal, triple))
  }

  override def evaluate(principal: Any, actions: util.Set[SecurityEvaluator.Action], graphIRI: Node): Boolean = {
    println("evaluate 3")
    actions.asScala forall (evaluate(principal, _, graphIRI))
  }

  override def evaluate(principal: Any,
                        actions:   util.Set[SecurityEvaluator.Action],
                        graphIRI:  Node,
                        triple:    graph.Triple): Boolean = {
    println("evaluate 4")
    (actions.asScala forall (evaluate(principal, _, graphIRI))) &&
    (principal.isPrivileged || evaluate(principal, triple))
  }

  private def evaluate(principal: Any, triple: Triple): Boolean = {
    println(s"triple: s -> ${triple.getSubject}; p -> ${triple.getPredicate}; o -> ${triple.getObject}")
    if (!principal.isAuthenticated) false
    else if (principal.isPrivileged) true
    else {
      val subject   = triple.getSubject
      val predicate = triple.getPredicate
      val obj       = triple.getObject

      if ((subject == Node.ANY) || (predicate == Node.ANY) || (obj == Node.ANY)) false
      else if (predicate.toString() == isPartOf.toString) principal isAuthorised obj
      else if (subject.isProjectId) principal isAuthorised subject
      else true
    }
  }

  private implicit class PrincipalOps(principal: Any) {

    private lazy val subject: Subject =
      Option(principal)
        .flatMap {
          case p: Subject => Some(p)
          case _ => None
        }
        .getOrElse(throw new AuthenticationRequiredException("No principal given"))

    lazy val isAuthenticated = subject.isAuthenticated
    lazy val isPrivileged: Boolean = PrivilegedUsers.contains(subject.getPrincipal.toString)

    def isAuthorised(node:  Node): Boolean = isAuthorised(node.toString)
    def isAuthorised(value: String): Boolean = {
      val r = subject hasRole value
      println(s"isAuthorised: $value -> $r")
      r
    }
  }

  private implicit class NodeOps(node: Node) {
    lazy val isProjectId: Boolean = node.toString startsWith projectIdPrefix
  }

  override def evaluateAny(principal: Any, actions: util.Set[SecurityEvaluator.Action], graphIRI: Node): Boolean = {
    println("evaluate any 1")
    evaluate(principal, actions, graphIRI)
  }

  override def evaluateAny(principal: Any,
                           actions:   util.Set[SecurityEvaluator.Action],
                           graphIRI:  Node,
                           triple:    graph.Triple): Boolean = {
    println(s"evaluate any 2: s -> ${triple.getSubject}; p -> ${triple.getPredicate}; o -> ${triple.getObject}")
    evaluate(principal, actions, graphIRI, triple)
  }

  override def evaluateUpdate(principal: Any, graphIRI: Node, from: graph.Triple, to: graph.Triple): Boolean = {
    println("evaluate update")
    true
  }

  override def getPrincipal: AnyRef = SecurityUtils.getSubject

  override def isPrincipalAuthenticated(principal: Any): Boolean = {
    println("isPrincipalAuthenticated")
    true
  }
}
