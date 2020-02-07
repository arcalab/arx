package dsl

import dsl.analysis.syntax.{FunDef, GroundTerm, Parser, Program}

import dsl.analysis.semantics.Encode.SemanticResult
import dsl.analysis.semantics._
import dsl.analysis.semantics.{Command, GuardedCommand}

import dsl.analysis.types._
import dsl.analysis.types.TVar

import dsl.backend.{Prelude, Prettify, Show, Simplify}

import dsl.common.{ParsingException, TypeException}


/**
  * Created by guillecledou on 2019-06-04
  */


object DSL {

  def parse(code:String):Program = Parser.parseProgram(code) match {
    case Parser.Success(result, _) => result
    case f:Parser.NoSuccess => throw new ParsingException("Parser failed: "+f)
  }

  def parseFunction(code:String):FunDef = Parser.parseFunction(code) match {
    case Parser.Success(result,next) => result match {
      case f@FunDef(n, ps, t, b) => f
      case _ => throw new RuntimeException("Only function defitions supported for now")
    }
    case f:Parser.NoSuccess => throw new ParsingException("Parser failed: "+f)
  }

  val prelude: Prelude.type = Prelude

  def unify(cons:Set[TCons]):(Map[TVar,TExp],Set[TCons]) = Unify(cons)

  def infer(program: Program):(Context,TExp,Set[TCons]) = Infer(program)

  def typeCheck(prog:Program):Context = {
    // mk type constraints
    val (ctx,t,cons) = infer(prog)
    // solve type constraints base on context
    val substitution = TypeCheck.solve(cons,ctx)
    // apply substitution to context
    val substCtx:Context = substitution(ctx)
    //add program type to context
    // todo: get type of the inputs
    val (program,programType) = ("program",FunEntry(TFun(TUnit,Simplify(substitution(t))),Context()))
    // prettify context
    Prettify.reset()
    val prettyCtx = Prettify(substCtx.add(program,programType))
    prettyCtx
  }

  /* DSL for Stream builders */

  class Var(name:String) {
    def :=(term:GroundTerm):Command = Command(name,term)
  }

  implicit def toVar(s:String):Var = new Var(s)

  def sb:StreamBuilder = StreamBuilder.empty

  def get(v:String):Guard = Get(v)
  def und(v:String):Guard = Und(v)
  def ask(v:String):Guard = Ask(v)

  def encode(prog:Program,typeCtx:Context):SemanticResult = Encode(prog,typeCtx)

}
