package dsl.analysis.types

import dsl.analysis.syntax._
import dsl.analysis.types.TypedProgram.TypedBlock
import dsl.backend.Import

/**
  * Created by guillecledou on 2020-02-07
  */


case class TypedProgram(imports:List[Import],types:List[TypeDecl],typedBlock:TypedBlock) {}

object TypedProgram {
  type TypedBlock = List[TypedStatement]
}

sealed abstract class TypedStatement
sealed abstract class TypedStreamExpr                                     extends TypedStatement
case class TypedFunDef(f:FunDef,t:TExp,tBlock:TypedBlock)                 extends TypedStatement
case class TypedSFunDef(sFunDef:SFunDef,t:TExp,tBlock:TypedStreamFun)     extends TypedStatement
case class TypedAssignment(asg:Assignment,tLhs:List[TExp],tRhs:TypedStreamExpr) extends TypedStatement

sealed abstract class TypedGroundTerm                                     extends TypedStreamExpr
case class TypedFunApp(tSFun:TypedStreamFun,t:TExp,tArgs:List[TypedGroundTerm])  extends TypedStreamExpr

case class TypedPort(p:String,t:TExp)              extends TypedGroundTerm
case class TypedConst(q:Const,t:TExp,tArgs:List[TypedGroundTerm])  extends TypedGroundTerm

sealed abstract class TypedStreamFun
case class TypedFunName(f:String,t:TExp)                    extends TypedStreamFun
case class TypedBuild(t:TExp,tArgs:TExp)                     extends TypedStreamFun
case class TypedMatch(t:TExp,tArgs:TExp)                     extends TypedStreamFun
case class TypedSeqFun(f1:TypedStreamFun, f2:TypedStreamFun) extends TypedStreamFun
case class TypedParFun(f1:TypedStreamFun, f2:TypedStreamFun) extends TypedStreamFun
