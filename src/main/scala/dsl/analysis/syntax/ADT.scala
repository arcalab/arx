package dsl.analysis.syntax

/**
  * Created by guillecledou on 2019-06-01
  */

sealed trait TypeName
case class AbsTypeName(name:String) extends TypeName {}
case class ConTypeName(name:String,param:List[TypeName]=List()) extends TypeName {}

sealed trait Variant {def name:String }
case class AdtVal(name:String) extends Variant {}
case class AdtConst(name:String, param:List[TypeName]) extends Variant {}



//object ADT {
//
//  def getType(t:AdtTerm,types:List[TypeDecl]):TypeExpr = {
//
//  }
//
//}

//sealed trait Type {}
//
//case class ADT(name:String,params:List[Type],variants:List[Variant]) extends Type
//case class AbsType(name:String) extends Type
//
//sealed trait Variant
//
//case class AdtVal(name:String) extends Variant {}
//case class AdtConst(name:String,params:List[Type]) extends Variant {}
