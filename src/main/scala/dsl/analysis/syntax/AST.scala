package dsl.analysis.syntax

import preo.ast.Connector

/**
  * Created by guillecledou on 2019-05-31
  */


sealed trait AST {
  def getTypes:List[TypeDecl] = this match {
    case t@TypeDecl(n,v) => List(t)
    case Statements(sts) => sts.flatMap(_.getTypes)
    case _ => List()
  }

  def getAssignments:List[Assignment] = this match {
    case a@Assignment(v,e) => List(a)
    case Statements(sts) => sts.flatMap(_.getAssignments)
    case _ => List()
  }

  def getDefs:List[ConnDef] =  this match {
    case c@ConnDef(name, con) => List(c)
    case Statements(sts) => sts.flatMap(_.getDefs)
    case _ => List()
  }
}

case class Statements(sts:List[AST]) extends AST {}

/* Type Declarations */

case class TypeDecl(name:TypeName,variants:List[Variant]) extends AST {}

/* Type Declarations - Type Names */
//case class ParametricTypeName(name:String) extends AST{}
//case class TypeName(name:String,param:List[TypeName]=List()) extends AST{}

/* Type Declarations - Variants */
//sealed trait Variant
//case class TypeVal(name:String) extends Variant {}
//case class TypeCons(name:String,param:List[TypeName]) extends Variant {}


/* Assignments */
//case class Identifier(name:String) extends AST {}
case class Assignment(variable:Identifier, expr:Expr) extends AST {}
case class MultAssignment(variables:List[Identifier],expr: Expr) extends AST {}

/* Expressions */

//case class AdtExp(expr:AST) extends AST {}
//case class AdtTerm(term:String) extends AST {}
//case class AdtConsExpr(cons:String,params:List[AST]) extends AST {}

/* Connector Definition */

case class ConnDef(name:String, c:Connector) extends AST {}
