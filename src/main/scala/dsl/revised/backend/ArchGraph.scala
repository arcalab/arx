package dsl.revised.backend

import dsl.revised.core.Network
import ArchGraph.{Port,Conn}

case class ArchGraph(ins: Map[Port,Set[Conn]], outs: Map[Port,Set[Conn]]):
  def in(p:Port,c:Conn)  = ArchGraph(ins + (p -> (ins.getOrElse(p,Set())+c)), outs)
  def out(p:Port,c:Conn) = ArchGraph(ins, outs + (p -> (outs.getOrElse(p,Set())+c)))

  def toMermaid:String =
    def fix(s:String): String = s.replaceAll("§","_")
    val inputs = for (p,cs)<-ins; c<-cs if !outs.contains(p) yield
      s"  ${fix(p)}_([${fix(p)}]):::inputClass --> ${c._1}"
    val outputs = for (p,cs)<-outs; c<-cs if !ins.contains(p) yield
      s"  ${c._1} --> ${fix(p)}_([${fix(p)}]):::outputClass"
    val io = for (p,cis)<-ins; ci<-cis; co<-outs.getOrElse[Set[Conn]](p,Set()); ci<-cis yield
      // p is an input of cs and an output of outs(p)
      s"  ${co._1} --> | ${fix(p)} | ${ci._1}"
    val nodes = for c<-(ins.values++outs.values).toSet.flatten yield
      s"  ${c._1}[ ${c._2} ]"
    s"flowchart TB\n${nodes.mkString("\n")}\n${io.mkString("\n")}\n${inputs.mkString("\n")}\n${outputs.mkString("\n")
       }\n  classDef inputClass fill:#4f4,stroke:#333,stroke-width:4px;\n  classDef outputClass fill:#f44,stroke:#333,stroke-width:4px;"

object ArchGraph:
  type Port = String // port name
  type Conn = (Int,String) // ID and name of connector instance

  def apply(n:Network): String  =
    var g = ArchGraph(Map(),Map())
    for (l,i) <- n.links.zipWithIndex do
      val ts = l.terms.map(t => t.toString).mkString(",")
      val conn = i -> s"${l.name}"//terms
      for in<-l.inputs do g=g.in(in,conn)
      for out<-l.outputs do g=g.out(out,conn)
    g.toMermaid


