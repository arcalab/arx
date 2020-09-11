package dsl

import scala.language.implicitConversions
import dsl.analysis.syntax.{FunDef, Parser, Program}
import dsl.analysis.semantics._
import dsl.analysis.semantics.Command
import dsl.analysis.types._
import dsl.analysis.types.TVar
import dsl.backend.{ArxNet, Prelude, Prettify, Show}
import dsl.common.ParsingException


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
      case _ => throw new RuntimeException("Only function definitions supported for now")
    }
    case f:Parser.NoSuccess => throw new ParsingException("Parser failed: "+f)
  }

//  def parseSB(code:String):StreamBuilder = SBParser.parseSB(code) match {
//    case SBParser.Success(result,_) => result
//    case f:SBParser.NoSuccess => throw new ParsingException("Parser failed: "+f)
//  }

  val prelude: Prelude.type = Prelude

  def unify(cons:Set[TCons]):(Map[TVar,TExp],Set[TCons]) = Unify(cons)

  def infer(program: Program):(Context,TExp,Set[TCons],TProgram) = Infer(program)

  def typeCheck(prog:Program):(TProgram,Context) = {
    // mk type constraints
    val (ctx,t,cons,tp) = infer(prog)
    // solve type constraints base on context
    val substitution = TypeCheck.solve(cons,ctx)
    // apply substitution to context and typed program, expanding destructors
    val (subsProgram,substCtx) = substitution(tp,ctx)
    // prettify context
    Prettify.reset()
    val prettyCtx = Prettify(substCtx)
    (Prettify(subsProgram),prettyCtx)
  }

  /* DSL for Stream builders */

  class Var(name:String) {
    def :=(term:Term):Command = Command(name,term)
  }

  implicit def toVar(s:String):Var = new Var(s)

  implicit def toGuard(gi:GuardItem):Guard = Guard(Set(gi))

  def sb:StreamBuilder = StreamBuilder.empty

  def get(v:String):Guard = Guard(Set(Get(v)))
  def und(v:String):Guard = Guard(Set(Und(v)))
  def ask(v:String):Guard = Guard(Set(Ask(v)))
  def isQ(n:String,v:Term):Guard = Guard(Set(IsQ(n,v)))

  //def encode(prog:TProgram, ctx:Context):SemanticResult =  Encode(prog,ctx)

  def encode(prog:TProgram, ctx:Context):SBContext = {
    val net = new ArxNet
    val (sb,sbOuts,sbCtx)  =  Encode(prog,ctx,net)
//    println("[DSL] Encoded net: "+net)
//    println("[DSL] Encoded sb: "+Show(sb))
    sbCtx.add("Program",(sb,List(),sbOuts,net)) // todo find program list of inputs when typechecking
  }


}
