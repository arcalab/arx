package dsl.revised.core

import dsl.revised.core.{Automaton, Rule, Term}
import Rule.Assignment
import Term._
import dsl.revised.core.Connector.{CAut, CNet}
import dsl.revised.core.Network.Link

object Show:
  def apply(t: Term): String = t match {
    case Var(v) => v
    case Fun(n, Nil) => s"$n"
    case Fun(n, List(t1,t2)) if n.forall("+-><!%/*=|&".toSet) => s"(${apply(t1)}$n${apply(t2)})"
    case Fun(n, ts) => s"$n(${ts.map(apply).mkString(",")})"
    case IntVal(i) => i.toString
  }

  def apply(a: Assignment): String =
    s"${a.v}:=${apply(a.t)}"

  private def showAssgEq(a: Assignment): String =
    s"${a.v}=${apply(a.t)}"

  def apply(r: Rule): String =
    val guard = List(r.get, r.ask, r.und, r.pred.map(apply))
      .zip(List("get", "ask", "und", ""))
      .filter(_._1.nonEmpty)
      .map((y, x) => if x != "" then s"$x(${y.mkString(",")})" else y.mkString(","))
      .mkString(",")
    val cmd = List(r.eqs.map(showAssgEq), r.upd.map(apply)) // a => apply(Assignment(a.v + "'", a.t))))
      .filter(_.nonEmpty)
      .map(_.mkString(","))
      .mkString(",")
    val lbls = if r.lbls.isEmpty then "" else r.lbls.mkString("[","; ","] ")
    lbls + guard + " --> " + cmd //+
//      s" [${r.highlights.mkString(",")}]"

  def apply(a: Automaton): String =
     s"${if a.init.nonEmpty then s"init: ${a.init.map(apply).mkString(",")};\n" else ""
        }${if a.clocks.nonEmpty then s"clock: ${a.clocks.mkString(",")};\n" else ""
        }${if a.inv.nonEmpty then s"inv: ${a.inv.map(apply).mkString(" && ")};\n" else ""
        }${if a.inputs.nonEmpty then s"input: ${a.inputs.mkString(",")};\n" else ""
        }${if a.outputs.nonEmpty then s"output: ${a.outputs.mkString(",")};\n" else ""
        }${if a.registers.nonEmpty then s"regs: ${a.registers.mkString(",")};\n" else ""
        }${if a.rs.nonEmpty then s"rules:\n${a.rs.map(r => s"  $r;").mkString("\n")}" else ""}"

  def apply(n:Network): String =
    (if n.data.nonEmpty then n.data.map(ppData).mkString("\n")+"\n\n" else "") +
    (if n.functions.nonEmpty then ppFun(n.functions)+"\n\n" else "") +
    (if n.connectors.nonEmpty then n.connectors.map(ppCon).mkString("\n")+"\n\n" else "") +
    (if n.links.nonEmpty then n.links.map(ppLnk).mkString("\n") else "")

  private def ppData(data: (String, (List[String], List[Network.Constructor]))): String =
    val (name,(args,constrs)) = data
    s"data $name ${args.mkString(" ")} = ${constrs.map(dsl.revised.syntax.Show.apply).mkString(" | ")};"

  private def ppFun(funs: Map[String,Interpretation]) = s"functions ${funs.keys.mkString(",")};"

  private def ppCon(conn: (String,Connector)): String = conn match
    case (name,CNet(n, args, ins, outs)) =>
      s"def $name${if args.nonEmpty then s"[${args.mkString(",")}]"else ""}(${
        ins.mkString(",")}) {\n" +
      dsl.revised.syntax.Show.indent(apply(n)+(if outs.nonEmpty then s"\nreturn ${outs.mkString(",")};" else "")) +
      "\n}"
    case (name,CAut(a, args, ins, outs)) =>
      s"aut $name${if args.nonEmpty then s"[${args.mkString(",")}]"else ""}(${
        ins.mkString(",")}) {\n" +
      dsl.revised.syntax.Show.indent(apply(a)+(if outs.nonEmpty then s"\nreturn ${outs.mkString(",")};" else "")) +
      "\n}"

  private def ppLnk(ln:Link): String =
    s"${ln.name}${if ln.terms.nonEmpty then s"[${ln.terms.map(apply).mkString(",")}]"else ""}(${
      ln.inputs.mkString(",")}) --> ${ln.outputs.mkString(",")};"

  def simple(n:Network): String =
    (if n.data.nonEmpty then n.data.map(ppData).mkString("\n")+"\n\n" else "") +
    (if n.functions.nonEmpty then ppFun(n.functions)+"\n\n" else "") +
    (if n.connectors.nonEmpty then n.connectors.toList.map(simple).mkString("\n")+"\n\n" else "") +
    (if n.links.nonEmpty then n.links.map(ppLnk).mkString("\n") else "")

  private def simple(conn: (String,Connector)): String = conn match
    case (name,CNet(n, args, ins, outs)) =>
      s"def $name${if args.nonEmpty then s"[${args.mkString(",")}]"else ""}(${
        ins.mkString(",")}) {...}"
    case (name,CAut(a, args, ins, outs)) =>
      s"aut $name${if args.nonEmpty then s"[${args.mkString(",")}]"else ""}(${
        ins.mkString(",")}) {...}"
