package dsl.backend

import dsl.analysis.semantics._
import dsl.analysis.syntax._
import dsl.analysis.syntax.ast._


/**
  * Created by guillecledou on 2019-06-07
  */

object Show {
  def apply(te:TypeExpr):String = te match {
    case TVar(n) => s"$n"
    case TMap(f, t) => apply(f) + " -> " + apply(t)
    case BaseType(n, ps) => n + (if (ps.isEmpty) "" else ps.map(apply).mkString("<",",",">"))
    case TUnit => "()"
    case TEithers(h,t) => "Either" + (h::t).map(apply).mkString("<",",",">")
    case TTuple(h,t) => (h::t).map(apply).mkString("(",",",")")
    case TProd(h,t) => (h::t).map(apply).mkString(" x ")
    case TOpt(t) => "Opt[" + apply(t) +  "]"
  }

  def apply(expr:Expr): String = expr match {
    case AdtTerm(name) => name
    case AdtConsExpr(name, params) => name + params.map(apply).mkString("(",",",")")
    case Identifier(name) => name
    case ConnId(name,ps) => name + (if (ps.isEmpty) "" else ps.map(apply).mkString("(",",",")"))
  }

  def apply(ast:AST): String = ast match {
    case Statements(sts) => sts.map(apply).mkString("\n")
    case Assignment(vs,expr) => vs.map(apply).mkString(",") + " = " + apply(expr)
    case TypeDecl(n,variants) => "type " + apply(n) + " = " + variants.map(apply).mkString(" | ")
    case ConnDef(n,c) => "connector " + n + "=" + preo.frontend.Show(c)
  }

  def apply(tname:TypeName):String = tname match {
    case AbsTypeName(n) => n
    case ConTypeName(n,ps) => n + (if (ps.isEmpty) "" else  ps.map(apply).mkString("<",",",">"))
  }

  def apply(variant:Variant):String = variant match {
    case AdtVal(n) => n
    case AdtConst(n, ps) => n + ps.map(apply).mkString("(",",",")")
  }
}
