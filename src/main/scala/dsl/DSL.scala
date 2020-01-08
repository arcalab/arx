package dsl

//import dsl.analysis.semantics.{Context, _}
import dsl.analysis.types._
import dsl.analysis.syntax.{AST, Parser2, Program}
import dsl.backend.Prettify
import dsl.common.{ParsingException, TypeException}
//import preo.ast.{Connector, CoreConnector}
//import preo.frontend.{Eval, Show, Simplify}
import dsl.analysis.types.TVar
/**
  * Created by guillecledou on 2019-06-04
  */


object DSL {

  def parse(code:String):Program = Parser2.parseProgram(code) match {
    case Parser2.Success(result, next) => result
    case f:Parser2.NoSuccess => throw new ParsingException("Parser failed: "+f)
  }

  def unify(cons:Set[TCons]):Map[TVar,TExp] = Unify1(cons)

  def infer(program: Program):(TContext,TExp,Set[TCons]) = Inference(program)

  def typeCheck(prog:Program):Map[String,TExp] = {
    // mk type constraints
    val (ctx,t,cons) = infer(prog)
    println("About to typecheck")
    // try to unify them
    val subst:Map[TVar,TExp] = Substitute(unify(cons))
    val substitution = Substitution(subst)
    // for each name in the context that is a fun return its type
    val functions:Map[String, ContextEntry] = ctx.functions
    val rawFunctionTypes = functions.map(f=>f._1-> substitution(f._2.tExp))
    var functionTypes = Map[String,TExp]()
    for((id,t) <- rawFunctionTypes) {
      Prettify.reset()
      functionTypes += id -> Prettify(t)
    }
    val rawPortsTypes:Map[String,TExp] = ctx.ports.map(p=>p._1->p._2.head.tExp).map(p=>p._1->substitution(p._2))
    var portsTypes = Map[String,TExp]()
    for((id,t) <- rawPortsTypes) {
      Prettify.reset()
      portsTypes += id -> Prettify(t)
    }
    portsTypes++functionTypes
  }

//  def typeCheck(ast: AST):Map[String,TypeExpr] = {
//    // mk type constraints
//    val (ctx,tconns,t,cons) = infer(ast)
//    // try to unify them
//    val substitutions:Map[TVar,TypeExpr] = Substitute(unify(cons))
//    // mk type of connector
//    val connTypes = tconns.map(c=> c._1->c._2.getType)
//    // for each typed identifier, get its typed and prettify it in case it has free type variables
//    val rawIdTypes = ctx.get.map(e => e._1 -> substitutions(e._2))
//    var idTypes = Map[String,TypeExpr]()
//    for((id,t) <- rawIdTypes) {
//      Prettify.reset()
//      idTypes += id -> Prettify(t)
//    }
//    // return the type for each identifier and for each connector definition
//    idTypes++connTypes
//  }


}
