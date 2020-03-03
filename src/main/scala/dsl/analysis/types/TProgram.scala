package dsl.analysis.types

import dsl.analysis.syntax._
import dsl.analysis.types.TProgram.TBlock
import dsl.backend.Import

/**
  * Created by guillecledou on 2020-02-07
  */

/**
  * A typed program with its corresponding typed context //todo: too much replication ?
  * @param imports user imports
  * @param userTypes  user defined types
  * @param tBlock typed block
  */
case class TProgram(imports:List[Import], userTypes:List[TypeDecl], tBlock:TBlock) {}

object TProgram {
  type TBlock = List[TStatement]
}

sealed abstract class TStatement
sealed abstract class TStreamExpr                                           extends TStatement
case class TFunDef(f:FunDef, t:TExp, tBlock:TBlock)                         extends TStatement
case class TSFunDef(sFunDef:SFunDef, t:TExp, tBlock:TStreamFun)             extends TStatement
case class TAssignment(asg:Assignment, tLhs:List[TExp], tRhs:TStreamExpr)   extends TStatement
case class TRAssignment(asg:RAssignment, tLhs:List[TExp], tRhs:TStreamExpr) extends TStatement

sealed abstract class TGroundTerm                                       extends TStreamExpr
case class TFunApp(tSFun:TStreamFun, tOut:TExp, tIn:List[TGroundTerm])  extends TStreamExpr

case class TPort(p:String, t:TExp)              extends TGroundTerm
case class TConst(q:Const, t:TExp, tIn:List[TGroundTerm])  extends TGroundTerm

sealed abstract class TStreamFun
case class TFunName(name:String, t:TExp,data:List[TGroundTerm]=List()) extends TStreamFun
case class TBuild(tIn:TExp, tOut:TExp)             extends TStreamFun
case class TMatch(tIn:TExp, tOut:TExp)             extends TStreamFun
case class TSeqFun(tf1:TStreamFun, tf2:TStreamFun) extends TStreamFun
case class TParFun(tf1:TStreamFun, tf2:TStreamFun) extends TStreamFun
