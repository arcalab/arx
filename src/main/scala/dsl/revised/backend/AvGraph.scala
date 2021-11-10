package dsl.revised.backend

import dsl.revised.Error
import dsl.revised.Error.Who
import dsl.revised.backend.AvGraph.Node
import dsl.revised.core.Network.Link
import dsl.revised.core.{Automaton, Connector, Network, Rule}

import scala.annotation.tailrec

/**
  * Builds an availability graph, where states denote what registers are avaliable (defined) and which are not.
  */

case class AvGraph(edges: Map[Node,Set[(Rule,Node)]]):
  def +(from:Node, to:Set[(Rule,Node)]) =
    val e2: Map[Node,Set[(Rule,Node)]] = edges + (from->to)
    AvGraph(e2)

object AvGraph:

  type Node = Set[String] // set of registers that are defined

  def apply(n:Network): AvGraph =
    apply(n.toAut)

  def allGraphs(n:Network): Set[AvGraph] =
    collectAut(n).map(apply)

  def apply(a:Automaton): AvGraph =
    val initNode = a.init.map(_.v).intersect(a.registers)
    buildGraph(Set(initNode),AvGraph(Map()))(using a)

  @tailrec
  def buildGraph(nodes:Set[Node],graph: AvGraph)(using a:Automaton): AvGraph =
    nodes.headOption match
      case None => graph
      case Some(node) =>
        if graph.edges contains node then buildGraph(nodes-node,graph)
        else
          val newEdges = a.rs // get rules
            .filter(r=>ready(r,node,a.registers)) // filter active ones
            .map(r=>r -> ((node--r.get)++(r.upd.map(_.v).intersect(a.registers)))) // update node for each one
          val newNodes = newEdges.map(_._2)
          buildGraph((nodes-node)++newNodes,graph+(node,newEdges))


  private def ready(r:Rule,node:Node,regs:Set[String]): Boolean =
    ((r.get++r.ask).intersect(regs) subsetOf node) &&
    !(r.und.exists(node))

  private def collectAut(net:Network): Set[Automaton] =
    given Who = Who("AGr")
    var auts = for Link(name,terms,inputs,outputs) <- net.links yield
      if !net.connectors.contains(name) then
        Error.encoding(s"Connector not found: '$name' among ${net.connectors.keys.mkString(",")}")
      net.connectors(name) match {
        case Connector.CNet(net2,args,ins,outs) =>
          collectAut(net2)
        case c:Connector.CAut =>
          Set(c.a)
      }
    auts.flatten.toSet
