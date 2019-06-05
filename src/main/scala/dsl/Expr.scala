package dsl

/**
  * Created by guillecledou on 2019-06-04
  */


sealed trait Expr{}

case class Identifier(name:String) extends Expr {}
case class AdtTerm(name:String) extends Expr {}
case class AdtConsExpr(name:String,params:List[Expr]) extends Expr {}
