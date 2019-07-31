package dsl.analysis.syntax.ast

/**
  * Created by guillecledou on 2019-06-01
  */

sealed trait TypeName {val name:String}
case class AbsTN(name:String) extends TypeName {}
case class ConTN(name:String, param:List[TypeName]=List()) extends TypeName {}

case class Constructor(name:String, param:List[TypeName]=List()) {}

// to remove:
sealed trait Variant {val name:String}
case class AdtVal(name:String) extends Variant {}
case class AdtConst(name:String, param:List[TypeName]=List()) extends Variant {}

