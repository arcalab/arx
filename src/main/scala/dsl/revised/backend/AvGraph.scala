package dsl.revised.backend

import dsl.revised.Error
import dsl.revised.Error.Who
import dsl.revised.backend.AvGraph.Node
import dsl.revised.core.Network.Link
import dsl.revised.core.{Automaton, Connector, Network, Rule, Term}

import scala.annotation.tailrec

/**
  * Builds an availability graph, where states denote what registers are avaliable (defined) and which are not.
  */

case class AvGraph(init:Node, edges: Map[Node,Set[(Rule,Node)]], inv:Set[Term]):
  def +(from:Node, to:Set[(Rule,Node)]) =
    val e2: Map[Node,Set[(Rule,Node)]] = edges + (from->to)
    AvGraph(init,e2,inv)

  def toMermaid(name:String): String =
    val name2 = if name=="" then "sync" else name
    def mkNodeId(n: Node) = s"${name2}_${n.toList.sorted.mkString("_")}"
    def mkNode(n:Node): String = s"${mkNodeId(n)}([ ${n.toList.sorted.mkString(",")} ])"
    def mkLbl(r:Rule): String =
      (r.toString)
      .replaceAll("\\|","_")
      .replaceAll("\\(","_")
      .replaceAll("\\)","")
      .replaceAll("-->","<br>")
      .replaceAll("\\[","--")
      .replaceAll("\\]","--<br>")

    val es = for e<-edges; target<-e._2 yield
      s"  ${mkNode(e._1)} --> |${mkLbl(target._1)}| ${mkNode(target._2)}"
    val es2 = for e<-edges if e._2.isEmpty yield
      s"  ${mkNode(e._1)}"
    val fst = s"style ${mkNodeId(init)} fill:#4f4,stroke:#333,stroke-width:4px"
    if edges.isEmpty then "subgraph $name2\n  empty([ ])\n  end"
    else s"subgraph $name2\n  direction TB\n${(es++es2).mkString("\n")}\n  $fst\n  end"



object AvGraph:

  type Node = Set[String] // set of registers that are defined

  def apply(n:Network): AvGraph =
    apply(n.toAut)

  def allGraphs(n:Network): Set[AvGraph] =
    collectAut(n).map(a=>apply(a._2))

  def allMermaid(n:Network): String =
    val auts = collectAut(n).map(aut => apply(aut._2).toMermaid(aut._1))
    s"flowchart LR\n${auts.mkString("\n")}"

  def apply(a:Automaton): AvGraph =
    val initNode = a.init.map(_.v).intersect(a.registers)
    buildGraph(Set(initNode),AvGraph(initNode,Map(),a.inv))(using a)

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

  private def collectAut(net:Network): Set[(String,Automaton)] =
    given Who = Who("AGr")
    var auts = for Link(name,terms,inputs,outputs) <- net.links yield
      if !net.connectors.contains(name) then
        Error.encoding(s"Connector not found: '$name' among ${net.connectors.keys.mkString(",")}")
      net.connectors(name) match {
        case Connector.CNet(net2,args,ins,outs) =>
          collectAut(net2)
        case c:Connector.CAut =>
          Set(name -> c.a)
      }
    auts.flatten.toSet
