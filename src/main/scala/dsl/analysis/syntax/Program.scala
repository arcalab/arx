package dsl.analysis.syntax

import dsl.analysis.syntax.Program.{Block, MaybeReactType, MaybeTypeName}
import dsl.analysis.types.RExp
import dsl.backend.{Import, Show}

case class Program(imports:List[Import],types: List[TypeDecl], block: Block) {
  override def toString: String = Show(this)
}
object Program {
  type Block = List[Statement]
  type MaybeTypeName = Option[TypeName]
  type MaybeReactType = Option[RExp]
}

sealed abstract class Statement
sealed abstract class StreamExpr        extends Statement
case class FunDef(name:String,
                  params:List[TypedVar],
                  typ: MaybeTypeName,
                  block:Block)         extends Statement
case class SFunDef(name:String,
                   typ: MaybeTypeName,
                   block:StreamFun)     extends Statement

case class Assignment(variables:List[String],
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

case class TypedVar(name:String,typ:MaybeTypeName,rtype:MaybeReactType)

// Data Type Declarations

sealed trait TypeName {val name:String}
case class AbsTypeName(name:String) extends TypeName
case class ConTypeName(name:String,param:List[TypeName]=List()) extends TypeName

case class TypeDecl(name:TypeName, constructors:List[Constructor])

case class Constructor(name:String, param:List[TypeName]=List()) {}
