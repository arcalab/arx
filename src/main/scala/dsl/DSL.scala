package dsl

import dsl.common.ParsingException
import dsl.analysis.semantics.TypeInference.Context
import dsl.analysis.semantics._
import dsl.analysis.syntax.{AST, Parser}

/**
  * Created by guillecledou on 2019-06-04
  */


object DSL {


  def parse(code:String):AST = Parser.parse(code) match {
    case Parser.Success(result, next) => result
    case f:Parser.NoSuccess => throw new ParsingException("Parser failed: "+f.msg)
  }

  def unify(cons:Set[TCons]):Map[TVar,TypeExpr] = Unify(cons)

  def infer(ast:AST):(Context,TypeExpr,Set[TCons]) = TypeInference.infer(ast)

  def typeCheck(ast: AST):Map[String,TypeExpr] = {
    // mk type constraints
    val (ctx,t,cons) = infer(ast)
    // try to unify them
    val substitutions:Map[TVar,TypeExpr] = Substitution(unify(cons))
    // return the type for each identifier
    ctx.get.map(e => e._1 -> substitutions(e._2))
  }

}
