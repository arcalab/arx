package dsl.analysis.syntax.ast

/**
  * Created by guillecledou on 2019-06-01
  */

sealed trait TypeName {val name:String}
case class AbsTypeName(name:String) extends TypeName {}
case class ConTypeName(name:String,param:List[TypeName]=List()) extends TypeName {}

sealed trait Variant {val name:String}
case class AdtVal(name:String) extends Variant {}
case class AdtConst(name:String, param:List[TypeName]=List()) extends Variant {}

case class Constructor(name:String, param:List[TypeName]=List()) {}