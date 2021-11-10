package dsl.revised

import dsl.revised.core.{Automaton, Term}
import dsl.revised.core.Network.Constructor
import dsl.revised.core.Term.{IntVal, Interpretation, falseT, trueT}
import dsl.revised.typing.MutTypeCtxt
import dsl.revised.typing.Type.{BaseType, VarType}
import dsl.revised.syntax.Program.Decl.{DataDecl,AutDecl}

object Prelude:

  /////////////////////////////
  // Interpretation of terms //
  /////////////////////////////

  private def arithm(op:(Int,Int)=>Int): Interpretation =
    case List(IntVal(t1),IntVal(t2)) => IntVal(op(t1,t2))

  /** Interpretations of some function symbols */
  val interpretations: Map[String,Interpretation] = Map(
    "+" -> arithm(_+_), "-" -> arithm(_-_), "*" -> arithm(_*_), "/" -> arithm(_/_),
    "==" -> {case List(t1,t2) => if t1==t2 then trueT else falseT},
    "!=" -> {case List(t1,t2) => if t1!=t2 then trueT else falseT},
    "!" -> {case List(`trueT`) => falseT; case List(`falseT`) => trueT}
  )

  ///////////
  // Types //
  ///////////

  val intType = BaseType("Int")
  val boolType = BaseType("Bool")
  val unitType = BaseType("Unit")

  private val at = VarType("a")

  val invFunction = Map(
    "at" -> (List(at),boolType),
    "notAt" -> (List(at),boolType)
  )

  val functions = Map(
    "==" -> (List(at,at),boolType), "!=" -> (List(at,at),boolType),
    ">=" -> (List(at,at),boolType), "<=" -> (List(at,at),boolType),
    ">"  -> (List(at,at),boolType), "<"  -> (List(at,at),boolType),
    "->" -> (List(boolType,boolType),boolType),
    "||" -> (List(boolType,boolType),boolType),
    "&&" -> (List(boolType,boolType),boolType),
    "!" -> (List(boolType),boolType),
    "+" -> (List(intType,intType),intType),
    "-" -> (List(intType,intType),intType),
    "*" -> (List(intType,intType),intType),
    "/" -> (List(intType,intType),intType),
    "()" -> (Nil,unitType),
    "True" -> (Nil,boolType),
    "False" -> (Nil,boolType)
  )

  ////////////////
  // Data types //
  ////////////////
  val data = List(
    DataDecl("Unit",Nil, List(Constructor("()",Nil))),
    DataDecl("Bool",Nil, List(Constructor("True",Nil),Constructor("False",Nil))),
    DataDecl("List",List("a"), List(
      Constructor("Cons",List(at,BaseType("List",List(at)))),
      Constructor("Nil",Nil)))
  )

  /** Default typing context with default function types */
  def newTypeContext = new MutTypeCtxt(functions = functions)


  //////////////////////////////
  // Reo primitive connectors //
  //////////////////////////////

  private val a="a";private val b="b";private val c="c";private val m="m";private val t="t"
  import core.Rule._
  import core.Term.{Var,Fun,~~,/~, :=}
  import scala.language.implicitConversions
  implicit def str2var(s:String): Var = Var(s)

  val syncModule = syntax.Program.Module(Nil,List(
        AutDecl("",Nil,List(a),List(b),Automaton(
          rs = Set( get(a) --> b~~a )))
  ))

  val reoModule = syntax.Program.Module(Nil, List(
//    AutDecl("",Nil,List(a),List(b),Automaton(
//      rs = Set( get(a) --> b~~a ))),
    AutDecl("fifo",Nil,List(a),List(b),Automaton(
      rs = Set( get(a) & und(m) --> m/~a,  get(m) --> b~~m ))),
    AutDecl("fifofull",List(c),List(a),List(b),Automaton(
      init=Set(m := c),rs = Set( get(a) & und(m) --> m/~a,  get(m) --> b~~m ))),
    AutDecl("var",Nil,List(a),List(b),Automaton(
      rs = Set( get(a) --> m/~a,  b~~m ))),
    AutDecl("varfull",List(c),List(a),List(b),Automaton(
      rs = Set( get(a) --> m/~a,  b~~m ), init = Set(m := c))),
    AutDecl("lossy",Nil,List(a),List(b),Automaton(
      rs = Set( get(a) --> b~~a,  get(a) ))),
    AutDecl("drain",Nil,List(a,b),Nil,Automaton(
      rs = Set( get(a,b) ))),
    AutDecl("xor",Nil,List(a),List(b,c),Automaton(
      rs = Set( get(a) --> b~~a,  get(a) --> c~~a ))),
    AutDecl("dupl",Nil,List(a),List(b,c),Automaton(
      rs = Set( get(a) --> b~~a & c~~a ))),
    AutDecl("merger",Nil,List(a,b),List(c),Automaton(
      rs = Set( get(a) --> c~~a,  get(b) --> c~~b ))),
    AutDecl("timer",List(c),List(a),List(b),Automaton(
      clocks = Set(t),
      inv = Set(Fun("->",List(Fun("at",List(m)),  Fun("<=",List(t,c)) ))),
      rs = Set(
        get(a) & und(m) --> m/~a & t/~IntVal(0),
        get(m) & pred(Fun(">=",List(t,c))) --> b~~m
      )))
  ))


