package dsl.analysis.syntax.ast

/**
  * Created by guillecledou on 2019-07-16
  */


sealed trait SE {}

sealed trait GT extends SE {}
case class Id(name:String) extends GT /*with SFun*/ {} //id
case class IdArgs(name:String, args:List[GT]) extends GT /*with SFun */{} // id (args)

case class Asg(ids:List[Id], e:SE) extends SE {} // ids := e
case class ParSE(e1:SE, e2:SE) extends SE {} // e1 e2
case class SFExpr(e:SF, args:List[GT]) extends SE {} // f ()
case class SFDef(name:String, tps:List[TypeName], dps:List[Id], body:SF) extends SE {} // def f<As><ids> = {f}
case class SEDef(name:String, tps:List[TypeName], dps:List[Id], ps:List[Id], body:SE) extends SE {} // def f<As><ids>(ids) = {e}

//sealed trait StreamExpr {}
//
//sealed trait GroundTerm extends StreamExpr {}
//case class Variable(name:String) extends GroundTerm  {}
//case class ConstExpr(name:String,args:List[GroundTerm]=List()) extends GroundTerm {}
//
//case class ParExpr(e1:StreamExpr,e2:StreamExpr) extends StreamExpr {}
//case class FunExpr(fun:StreamFun,args:List[GroundTerm]) extends StreamExpr {}
//case class Assig(vars:List[Variable], e:StreamExpr) extends StreamExpr {}
//
//sealed trait FunDefinition  extends StreamExpr {}
//case class FunSEDef(name:String, typeParams:List[TypeName], dataParams:List[Variable], params:List[Variable], body:StreamExpr) extends FunDefinition {}
//case class FunSFDef(name:String, typeParams:List[TypeName], dataParams:List[Variable], body:StreamFun) extends FunDefinition {}
