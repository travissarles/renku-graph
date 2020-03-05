package io.renku.jena.evaluator

import java.util

import org.apache.jena.graph
import org.apache.jena.graph.Node
import org.apache.jena.permissions.SecurityEvaluator
import org.apache.shiro.SecurityUtils

class RenkuEvaluator(smth: Any) extends SecurityEvaluator {

  println(s"inited with $smth")

  override def evaluate(principal: Any, action: SecurityEvaluator.Action, graphIRI: Node): Boolean = {
    println("evaluate 1")
    true
  }

  override def evaluate(principal: Any,
                        action:    SecurityEvaluator.Action,
                        graphIRI:  Node,
                        triple:    graph.Triple): Boolean = {
    println("evaluate 2")
    true
  }

  override def evaluate(principal: Any, actions: util.Set[SecurityEvaluator.Action], graphIRI: Node): Boolean = {
    println("evaluate 3")
    true
  }

  override def evaluate(principal: Any,
                        actions:   util.Set[SecurityEvaluator.Action],
                        graphIRI:  Node,
                        triple:    graph.Triple): Boolean = {
    println("evaluate 4")
    true
  }

  override def evaluateAny(principal: Any, actions: util.Set[SecurityEvaluator.Action], graphIRI: Node): Boolean = {
    println("evaluate any 1")
    true
  }

  override def evaluateAny(principal: Any,
                           actions:   util.Set[SecurityEvaluator.Action],
                           graphIRI:  Node,
                           triple:    graph.Triple): Boolean = {
    println("evaluate any 2")
    true
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
