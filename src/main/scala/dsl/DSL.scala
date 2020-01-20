package dsl

//import dsl.analysis.semantics.{Context, _}
import dsl.analysis.types._
import dsl.analysis.syntax.{Parser, Program}
import dsl.backend.{Prettify, Show, Simplify}
import dsl.common.{ParsingException, TypeException}
//import preo.ast.{Connector, CoreConnector}
//import preo.frontend.{Eval, Show, Simplify}
import dsl.analysis.types.TVar
/**
  * Created by guillecledou on 2019-06-04
  */


object DSL {

  def parse(code:String):Program = Parser.parseProgram(code) match {
    case Parser.Success(result, next) => result
    case f:Parser.NoSuccess => throw new ParsingException("Parser failed: "+f)
  }

  def unify(cons:Set[TCons]):Map[TVar,TExp] = Unify(cons)

  def infer(program: Program):(TContext,TExp,Set[TCons]) = Infer(program)

  def typeCheck(prog:Program):Map[String,TExp] = {
    // mk type constraints
    val (ctx,t,cons) = infer(prog)
//    println("About to typecheck")
//    println("Type constraints:\n"+cons.map(c=>Show(c)).mkString("\n"))
    // try to unify them
    val subst:Map[TVar,TExp] = Substitute(unify(cons))
    val substitution = Substitution(subst)
    // for each name in the context that is a fun return its type
    val functions:Map[String, ContextEntry] = ctx.functions
      .filterNot(f=> Set("fifo","dupl","lossy","merger","xor","drain","writer","reader").contains(f._1))
    val rawFunctionTypes = functions.map(f=>f._1-> Simplify(substitution(f._2.tExp)))
    var functionTypes = Map[String,TExp]()
    Prettify.reset()
    for((id,t) <- rawFunctionTypes) {
//      Prettify.reset()
      functionTypes += id -> Prettify(t)
    }
    // ports types
    val rawPortsTypes:Map[String,TExp] = ctx.ports.map(p=>p._1->p._2.head.tExp).map(p=>p._1->Simplify(substitution(p._2)))
    var portsTypes = Map[String,TExp]()
//    Prettify.reset()
    for((id,t) <- rawPortsTypes) {

      portsTypes += id -> Prettify(t)
    }
    // program types
//    Prettify.reset()
    val programType = Map("program" -> Prettify(Simplify(substitution(t))))

    programType++functionTypes++portsTypes
    //rawFunctionTypes++rawPortsTypes
  }

}
