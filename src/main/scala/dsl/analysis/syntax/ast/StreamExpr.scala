package dsl.analysis.syntax.ast

/**
  * Created by guillecledou on 2019-07-16
  */

sealed trait StreamExpr {}

sealed trait GroundTerm extends StreamExpr {}
case class Variable(name:String) extends GroundTerm  {}
case class ConstExpr(name:String,args:List[GroundTerm]=List()) extends GroundTerm {}

case class ParExpr(e1:StreamExpr,e2:StreamExpr) extends StreamExpr {}
case class FunExpr(fun:StreamFun,args:List[GroundTerm]) extends StreamExpr {}
case class Assig(vars:List[Variable], e:StreamExpr) extends StreamExpr {}

sealed trait FunDefinition  extends StreamExpr {}
case class FunSEDef(name:String, typeParams:List[TypeName], dataParams:List[Variable], params:List[Variable], body:StreamExpr) extends FunDefinition {}
case class FunSFDef(name:String, typeParams:List[TypeName], dataParams:List[Variable], body:StreamFun) extends FunDefinition {}
