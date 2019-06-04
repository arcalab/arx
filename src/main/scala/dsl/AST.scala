package dsl

/**
  * Created by guillecledou on 2019-05-31
  */


trait AST {
  def getAdts:List[TypeDecl] = this match {
    case t@TypeDecl(n,v) => List(t)
    case Statements(sts) => sts.flatMap(_.getAdts)
    case _ => List()
  }
}

case class Statements(sts:List[AST]) extends AST {}

/* Type Declarations */

//case class TypeDecl(name:String,variants:List[AST],param:List[AST]=List()) extends AST {}
case class TypeDecl(name:TypeName,variants:List[AST]) extends AST {}
case class ParametricTypeName(name:String) extends AST{}
case class TypeName(name:String,param:List[AST]=List()) extends AST{}
case class TypeVal(name:String) extends AST {}
case class TypeCons(name:String,param:List[AST]) extends AST {}


/* Assignments */
case class Identifier(name:String) extends AST {}
case class Assignment(variable:Identifier,expr:AST) extends AST {}

/* Expressions */

//case class AdtExp(expr:AST) extends AST {}
case class AdtTerm(term:String) extends AST {}
case class AdtConsExpr(cons:String,params:List[AST]) extends AST {}

