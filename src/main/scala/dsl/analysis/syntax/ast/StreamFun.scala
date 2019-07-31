package dsl.analysis.syntax.ast

/**
  * Created by guillecledou on 2019-07-16
  */

sealed trait SF{}

case class SeqSF(f1:SF, f2:SF) extends SF {} // f ; f
case class ParSF(f1:SF, f2:SF) extends SF {} // f  f


case class GExpr(e:SF) extends SF {} // {e}
case class FName(name:String,tps:List[TypeName],dps:List[GT]) extends SF {} //id[<A>][<GT>]

case class FBuild(args:Option[TypeName]) extends SF {}
case class FMatch(args:Option[TypeName]) extends SF {}


//trait StreamFun {}
//
//case class FunName(name:String) extends StreamFun {}
//case class ParFun(f1:StreamFun,f2:StreamFun) extends StreamFun {}
//case class SeqFun(f1: StreamFun,f2:StreamFun) extends StreamFun {}
//case class Build(args:Option[TypeName]) extends StreamFun {}
//case class Match(args:Option[TypeName]) extends StreamFun {}

//case class Opt(pattern:Pattern,e:StreamExpr) {}
//
//sealed trait Pattern {}
//
//case object Wildcard extends Pattern {}
//case class GroundPattern(g:GroundTerm) extends Pattern {}



