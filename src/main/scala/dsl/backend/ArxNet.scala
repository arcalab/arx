package dsl.backend

import dsl.backend.ArxNet.Edge
import scala.collection.mutable


object ArxNet {
  case class Edge(from:Set[String],to:Set[String],prim:String) {
    override def toString: String =
      s"${from.mkString("[",",","]")} -($prim)-> ${to.mkString("[",",","]")}"
  }
}

class ArxNet()  {
  protected val edges:mutable.Set[Edge]= mutable.Set()
  protected val mirrors: mutable.Map[String,Set[String]] = mutable.Map()

  def getEdges: Set[Edge] = edges.toSet
  def mirror(n:String):Set[String] = mirrors.getOrElse(n,Set())

  override def toString: String =
    s" - Edges:\n${edges.mkString("\n")}\n - Mirrors:\n${mirrors.mkString("\n")}"

  override def clone(): ArxNet = {
    val e = edges
    val m = mirrors
    new ArxNet() {
      override protected val edges: mutable.Set[Edge] = e.clone()
      override protected val mirrors: mutable.Map[String, Set[String]] = m.clone()
    }
  }

  /** Add edge */
  def +=(e:Edge): ArxNet = {
    edges += e
    this
  }
  /** Add edge */
  def +=(from:Set[String],to:Set[String],prim:String): ArxNet =
    this += Edge(from,to,prim)

  /** Remove edge */
  def -=(e:Edge): ArxNet = {
    edges -= e
    this
  }

  /** Add mirror */
  def +=(from:String,to:String): ArxNet = {
    mirrors += from -> (mirrors.getOrElse(from,Set()) + to)
    this
  }

  def ++=(other:ArxNet): ArxNet = {
    edges ++= other.edges
    mirrors ++= other.mirrors
    this
  }

  def replace(map:Map[String,String]): ArxNet = {
    val e2 = edges.map(e => Edge(
      for (f <- e.from) yield map.getOrElse(f,f),
//      e.from.map(y => map.getOrElse(y,y)),
      e.to.map(y => map.getOrElse(y,y)),
      e.prim
    ))
    edges.clear()
    edges ++= e2
    val m2 = mirrors.map(kv => map.getOrElse(kv._1,kv._1) ->
      kv._2.map(y => map.getOrElse(y,y)))
    mirrors.clear()
    mirrors ++= m2
    this
  }

  def replace(x:String,by:String): ArxNet = {
    edges.map(e=>Edge(
      e.from.map(y=> if (y==x) by else y),
      e.to.map(y=> if (y==x) by else y),
      e.prim
    ))
    mirrors.map(kv => (if (kv._1==x) by else kv._1) ->
                      kv._2.map(y=> if (y==x) by else y))
    this
  }
}
