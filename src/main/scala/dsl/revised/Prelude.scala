package dsl.revised

import dsl.revised.core.Network.Constructor
import dsl.revised.core.Term.{IntVal, Interpretation, falseT, trueT}
import dsl.revised.syntax.Program.DataDecl
import dsl.revised.typing.MutTypeCtxt
import dsl.revised.typing.Type.{BaseType, VarType}

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

  private val a = VarType("a")
  private val b = VarType("b")

  val invFunction = Map(
    "at" -> (List(a),boolType)
  )

  val functions = Map(
    "==" -> (List(a,a),boolType), "!=" -> (List(a,a),boolType),
    ">=" -> (List(a,a),boolType), "<=" -> (List(a,a),boolType),
    ">"  -> (List(a,a),boolType), "<"  -> (List(a,a),boolType),
    "->" -> (List(a,a),boolType),
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
      Constructor("Cons",List(a,BaseType("List",List(a)))),
      Constructor("Nil",Nil)))
  )


  /** Default typing context with default function types */
  def newTypeContext = new MutTypeCtxt(functions = functions)
