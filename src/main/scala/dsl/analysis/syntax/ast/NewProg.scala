package dsl.analysis.syntax.ast

/**
  * Created by guillecledou on 2019-07-30
  */


case class NewProg(ds:List[TD],e:SExpr) {}

case class TD(name:TypeName, constr:List[Constructor]) {}

sealed trait SExpr {}

sealed trait GT extends SExpr {}
case class Id(name:String) extends GT /*with SFun*/ {} //id
case class IdArgs(name:String, args:List[GT]) extends GT /*with SFun */{} // id (args)

case class Asg(ids:List[Id], e:SExpr) extends SExpr {} // ids := e

case class ParSExpr(e1:SExpr,e2:SExpr) extends SExpr {} // e1 e2

sealed trait SFun{}

case class SeqSFun(f1:SFun,f2:SFun) extends SFun {} // f ; f
case class ParSFun(f1:SFun,f2:SFun) extends SFun {} // f  f

case class FExpr(e:SFun,args:List[GT]) extends SExpr {} // f ()
case class GExpr(e:SFun) extends SFun {} // {e}
case class FName(name:String,tps:List[TypeName],dps:List[GT]) extends SFun {} //id<A><GT>

case class FBuild(args:Option[TypeName]) extends SFun {}
case class FMatch(args:Option[TypeName]) extends SFun {}

case class FFDef(name:String, tps:List[TypeName], dps:List[Id], body:SFun) extends SExpr {} // def f<As><ids> = {f}
case class FEDef(name:String, tps:List[TypeName], dps:List[Id], ps:List[Id],body:SExpr) extends SExpr {} // def f<As><ids>(ids) = {e}



