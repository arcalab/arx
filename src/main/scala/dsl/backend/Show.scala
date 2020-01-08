package dsl.backend

//import dsl.analysis.semantics._
import dsl.analysis.syntax._
import dsl.analysis.types._


/**
  * Created by guillecledou on 2019-06-07
  */

object Show {
//  def apply(te:TypeExpr):String = te match {
//    case TVar(n) => s"$n"
//    case TMap(f, t) => apply(f) + " -> " + apply(t)
//    case BaseType(n, ps) => n + (if (ps.isEmpty) "" else ps.map(apply).mkString("<",",",">"))
//    case TUnit => "()"
//    case TEithers(h,t) => "Either" + (h::t).map(apply).mkString("<",",",">")
//    case TTuple(h,t) => (h::t).map(apply).mkString("(",",",")")
//    case TProd(h,t) => (h::t).map(apply).mkString(" x ")
//    case TOpt(t) => "Opt[" + apply(t) +  "]"
//  }

  def apply(te:TExp):String = te match {
    case TVar(n) => s"$n"
    //    case TMap(f, t) => apply(f) + " -> " + apply(t)
    case TFun(ins,outs) => apply(ins) + " -> " + apply(outs)
    case TInterface(l) => l.map(apply).mkString(" x ")
    case TBase(n, ps) => n + (if (ps.isEmpty) "" else ps.map(apply).mkString("<",",",">"))
    case TUnit => "()"
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
    case FunDef(name, expr, params) => s"$name<${params.map(_.name).mkString(",")}> = ${apply(expr)}"
    //case ConnDef(n,c) => "connector " + n + "=" + preo.frontend.Show(c)
  }

  def apply(tname:TypeName):String = tname match {
    case AbsTypeName(n) => n
    case ConTypeName(n,ps) => n + (if (ps.isEmpty) "" else  ps.map(apply).mkString("<",",",">"))
  }

  def apply(variant:Variant):String = variant match {
    case AdtVal(n) => n
    case AdtConst(n, ps) => n + ps.map(apply).mkString("(",",",")")
  }

  def apply(variant:Constructor):String =
    variant.name + variant.param.map(apply).mkString("(",",",")")


  //////////////////
  def apply(p:Program): String =
    p.types.map(apply).mkString("\n") +
      (if (p.types.nonEmpty) "\n\n" else "") +
      p.block.map(apply).mkString("\n")

  def apply(td: TypeDecl2): String =
    "data " + apply(td.name) + " = " + td.constructors.map(apply).mkString(" | ")

  def apply(s: Statement)(implicit ind:Int = 0): String = fwd(ind) + (s match {
    case Assignment2(variables, expr) =>
      variables.mkString(",")+" := "+apply(expr)(0)
    case FunDef2(name, params, typ, block) =>
      "def "+name+"("+params.map(apply).mkString(",")+")"+
        (if (typ.isDefined) " : "+apply(typ.get) else "")+
        " = {\n"+block.map(s=>apply(s)(ind+1)+"\n").mkString+
        fwd(ind)+"}"
    case expr: StreamExpr => expr match {
      case FunctionApp(sfun, args) => apply(sfun)+"("+args.map(s=>apply(s)(0)).mkString(",")+")"
      case term: GroundTerm => term match {
        case Const(q, args) => q+(if (args.nonEmpty) "("+args.map(s=>apply(s)(0)).mkString(",")+")" else "")
        case Port(x) => x
      }
    }
  })

  def apply(tv:TypedVar): String =
    tv.name+(tv.typ match {
      case Some(t) => ":"+apply(t)
      case None => ""
    })
  def apply(fun: StreamFun): String = fun match {
    case FunName(f) => f
    case Build => "build"
    case Match => "match"
  }


  private def fwd(i: Int): String = "  "*i
}
