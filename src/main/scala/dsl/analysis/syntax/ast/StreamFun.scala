package dsl.analysis.syntax.ast

/**
  * Created by guillecledou on 2019-07-16
  */


sealed trait StreamFun {}

case class FunName(name:String) extends StreamFun {}
case class ParFun(f1:StreamFun,f2:StreamFun) extends StreamFun {}
case class SeqFun(f1: StreamFun,f2:StreamFun) extends StreamFun {}
case class Build(args:Option[TypeName]) extends StreamFun {}
case class Match(args:Option[TypeName]) extends StreamFun {}

//case class Opt(pattern:Pattern,e:StreamExpr) {}
//
//sealed trait Pattern {}
//
//case object Wildcard extends Pattern {}
//case class GroundPattern(g:GroundTerm) extends Pattern {}



