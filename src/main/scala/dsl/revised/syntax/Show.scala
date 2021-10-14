package dsl.revised.syntax

import Program._
import dsl.revised.core.Network.Constructor

object Show:
  def apply(program: Program): String =
    if program.modules.isEmpty then
      apply(program.main)
    else
      program.modules.map(x => s"module ${x._1}:\n\n${apply(x._2)}").mkString("\n\n") +
      s"module main:\n\n${apply(program.main)}"

  def apply(m:Module): String =
    val imp = if m.imports.nonEmpty then s"import ${m.imports.mkString(", ");}\n\n" else ""
    val decl = m.declarations.map(apply).mkString("\n\n")
    imp+decl

  def apply(d:Decl): String = d match
    case DataDecl(name, args, const) => s"data $name ${
      if args.nonEmpty then s"<${args.mkString(",")}> " else ""} = ${const.map(apply).mkString(" | ")};"

    case AutDecl(name, args, inputs, outputs, aut) =>
      s"aut $name${
        if args.nonEmpty then s"<${args.mkString(",")}>" else ""}(${inputs.mkString(",")}) {"+
      indent(s"\n$aut${
        if outputs.nonEmpty then s"\nreturn ${outputs.mkString(",")};" else ""}") +
      "\n}"

    case NetDecl(name, args, inputs, outputs, decls) =>
      s"def $name${
        if args.nonEmpty then s"<${args.mkString(",")}>" else ""}(${inputs.mkString(",")}) {\n" +
      indent(s"${
        decls.map(apply).mkString("\n")}${
        if outputs.nonEmpty then s"\nreturn ${outputs.mkString(",")};" else ""}") +
      "\n}"

    case LinkDecl(invoc, outputs) =>
      s"${apply(invoc)} --> ${if outputs.nonEmpty then outputs.mkString(",") else "()"}"


  def apply(c:Constructor): String =
    c.name + (if c.args.nonEmpty then s"(${c.args.mkString(",")}" else "")

  def apply(call: InputCall): String = call match
    case PortCall(n) => n
    case ConnCall(name, args, inputs) => s"$name${
      if args.nonEmpty then s"<${args.map(dsl.revised.core.Show.apply).mkString(",")}>" else ""}(${
      inputs.map(apply).mkString(",")})"

  def indent(str:String,n:Int=1): String =
    val s = "  ".repeat(n)
    s + str.replaceAll("\n",s"\n${s}")

