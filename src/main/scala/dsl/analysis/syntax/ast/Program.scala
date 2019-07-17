package dsl.analysis.syntax.ast

/**
  * Created by guillecledou on 2019-07-16
  */

case class Program(adts:List[TypeDeclaration],funs:List[FunDefinition],exprs:List[StreamExpr]) {}

case class TypeDeclaration(name:TypeName,variants:List[Variant]) {}

sealed trait FunDefinition  {}
case class FunSEDef(name:String,typeParams:List[TypeName],params:List[Variable], body:StreamExpr) extends FunDefinition {}
case class FunSFDef(name:String,typeParams:List[TypeName],body:StreamFun) extends FunDefinition {}
