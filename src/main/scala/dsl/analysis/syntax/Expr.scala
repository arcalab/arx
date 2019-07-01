package dsl.analysis.syntax

/**
  * Created by guillecledou on 2019-06-04
  */


sealed trait Expr {
  def isVariable:Boolean = this match {
    case Identifier(n) => true
    case _=> false
  }
}

case class Identifier(name:String) extends Expr {}
case class AdtTerm(name:String) extends Expr {}
case class AdtConsExpr(name:String,params:List[Expr]) extends Expr {}
case class ConnId(name:String, params:List[Expr]=List()) extends Expr {}
