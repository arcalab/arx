package dsl.analysis.semantics.revised

import dsl.analysis.semantics.revised.Rule.Assignment
import dsl.analysis.semantics.revised.Term._

object Show:
  def apply(t:Term): String = t match {
    case Var(v) => v
    case TFun(n,ts) => s"$n(${ts.map(apply).mkString(",")})"
    case TInt(i) => i.toString
  }

  def apply(a:Assignment): String =
    s"${a.v}:=${apply(a.t)}"

  def apply(r:Rule): String =
    val guard = List(r.get,r.ask,r.und,r.pred.map(apply))
      .zip(List("get","ask","und",""))
      .filter(_._1.nonEmpty)
      .map((y,x) => if x!="" then s"$x(${y.mkString(",")})" else y.mkString(","))
    val cmd =  List(r.assg.map(apply),r.upd.map(a=>apply(Assignment(a.v+"'",a.t))))
    guard.mkString(",") + " --> "+
      cmd.filter(_.nonEmpty).map(_.mkString(",")).mkString(",") +
      s" [${r.highlights.mkString(",")}]"

  def apply(a:Automaton): String =
    s"""init: ${a.init.map(apply).mkString(",")}
       |input: ${a.inputs.mkString(",")}
       |output: ${a.outputs.mkString(",")}
       |regs: ${a.registers.mkString(",")}
       |rules:
       |${a.rs.map(r => s" - $r").mkString("\n")}""".stripMargin
