package dsl.analysis.syntax

import dsl.analysis.syntax.Program.{Block, TypeName2}
import dsl.backend.Show

case class Program(types: List[TypeDecl2], block: Block) {
  override def toString: String = Show(this)
}
object Program {
  type Block = List[Statement]
  type TypeName2 = Option[TypeName]
}

sealed abstract class Statement
sealed abstract class StreamExpr        extends Statement
case class FunDef2(name:String,
                   params:List[TypedVar],
                   typ: TypeName2,
                   block:Block)         extends Statement
case class SFunDef(name:String,
                   typ: TypeName2,
                   block:StreamFun)     extends Statement

case class Assignment2(variables:List[String],
                       expr:StreamExpr) extends Statement

sealed abstract class GroundTerm               extends StreamExpr
case class FunctionApp(sfun: StreamFun,
                       args: List[GroundTerm]) extends StreamExpr

case class Port(x:String)               extends GroundTerm
case class Const(q:String,
                 args:List[GroundTerm]) extends GroundTerm

sealed abstract class StreamFun
case class FunName(f:String) extends StreamFun
case object Build            extends StreamFun
case object Match            extends StreamFun
case class SeqFun(f1:StreamFun, f2:StreamFun) extends StreamFun
case class ParFun(f1:StreamFun, f2:StreamFun) extends StreamFun

case class TypedVar(name:String,typ:TypeName2)

// Data Type Declarations

sealed trait TypeName {val name:String}
case class AbsTypeName(name:String) extends TypeName
case class ConTypeName(name:String,param:List[TypeName]=List()) extends TypeName

case class TypeDecl2(name:TypeName, constructors:List[Constructor])

case class Constructor(name:String, param:List[TypeName]=List()) {}
