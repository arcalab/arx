package dsl.analysis.types

import dsl.analysis.syntax.Statement
import dsl.backend.{In, Out, Show}
import dsl.common.TypeException

/**
  * Created by guillecledou on 2020-01-07
  */


object Check {

  def numParams(actual:Int,formal:Int):Unit =
    if (actual!=formal) throw new TypeException(s"Expected ${formal} inputs, but ${actual} found")

  def isFunType(expr: TExp):TFun = expr match {
    case t@TFun(ins,outs) => t
    case e => throw new TypeException(s"Function type expected but ${e.getClass} found")
  }

  def isInterfaceType(expr:TExp):TExp = expr match {
    case TFun(_,_) => throw new TypeException(s"Interface type expected but Function type found")
    case _ => expr
  }

  def lhsAssigAreVars(vars:List[String],ctx:TContext):Unit = {
    var err: Option[ContextEntry] = None
    vars.exists(v =>
      if (ctx.context.contains(v) && !ctx.ports.contains(v)) {err = Some(ctx.context(v)); true} else false)
    if (err.isDefined)
      throw new TypeException(s"Only variables can be used on the LHS of an assignment but ${err.getClass} found")
    if (vars.toSet.size != vars.size)
      throw new TypeException(s"Cannot repeat variables on the LHS of an assignment but ${vars.mkString(",")} found")
  }

  def isDestr(t:TExp):Boolean = t match {
    case TDestr(t1) => true
    case _ => false
  }

  def isClosed(s:Statement,ports:Map[String,List[PortEntry]]):Unit = {
    //ports.forall(p=> isClosedPort(p._2))
    var err: Option[String] = None
    ports.forall(p=> if (isClosedPort(p._2)) true else {err=Some(p._1);false})
    if (err.isDefined)
      throw new TypeException(s"Port ${err.get} is not closed in:\n ${Show(s)}")
  }
  private def isClosedPort(occurrences:List[PortEntry]):Boolean = {
    val in = occurrences.find(p => p.pType == In)
    val out = occurrences.find(p=> p.pType == Out)
    in.isDefined && out.isDefined
  }



}
