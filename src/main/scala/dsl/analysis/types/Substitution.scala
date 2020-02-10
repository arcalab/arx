package dsl.analysis.types

import dsl.analysis.syntax._
import dsl.analysis.types.TypedProgram.TypedBlock
import dsl.backend.Simplify

/**
  * Created by guillecledou on 2019-08-05
  */


case class Substitution(sub:Map[TVar,TExp]) {
  def add(t:TVar,te: TExp) = Substitution(sub+(t->te))

  def apply(te:TExp): TExp = te match {
    case TUnit => TUnit
    case t@TVar(n) => if (sub.contains(t)) sub(t) else t
    case TFun(ins,outs) => TFun(apply(ins),apply(outs))
    case TTensor(t1,t2) => TTensor(apply(t1),apply(t2))
    case TBase(n,ps) => TBase(n,ps.map(apply))
    case TDestr(t1) => TDestr(apply(t1))
  }

  def apply(ctx:Context):Context = {
    val nfuns = ctx.functions.map(f=>
      f._1 -> FunEntry(TFun(Simplify(apply(f._2.tExp.tIn)),Simplify(apply(f._2.tExp.tOut))),apply(f._2.funCtx)))
    val nports = ctx.ports.map(p=>p._1 -> p._2.map(pe=>PortEntry(apply(pe.tExp),pe.pType)))
    Context(ctx.adts,nfuns,nports)
  }

  def apply(tp:TypedProgram,ctx:Context):(TypedProgram,Context) = {
    val nctx = apply(ctx)
    (TypedProgram(tp.imports,tp.types,apply(tp.typedBlock,nctx)),nctx)
  }

  def apply(tb:TypedBlock,ctx:Context):TypedBlock = tb.map(b=>apply(b,ctx))

  def apply(ts:TypedStatement,ctx:Context):TypedStatement = ts match {
    case TypedFunDef(f,t,tb) => TypedFunDef(f,apply(t),apply(tb,ctx))
    case TypedSFunDef(f,t,tb) => TypedSFunDef(f,apply(t),apply(tb,ctx))
    case TypedAssignment(a,tlhs,trhs) => TypedAssignment(a,tlhs.map(apply),apply(trhs,ctx))
    case se:TypedStreamExpr => apply(se,ctx)
  }

  def apply(tsf:TypedStreamFun,ctx:Context):TypedStreamFun = tsf match {
    case TypedFunName(f,t)  => TypedFunName(f,apply(t))
    case TypedBuild(t,ta)   => TypedBuild(Destructor.expand(apply(t),ctx),Destructor.expand(apply(ta),ctx))
    case TypedMatch(t,ta)   => TypedMatch(apply(t),apply(ta))
    case TypedSeqFun(t1,t2) => TypedSeqFun(apply(t1,ctx),apply(t2,ctx))
    case TypedParFun(t1,t2) => TypedParFun(apply(t1,ctx),apply(t2,ctx))
  }

  def apply(tse:TypedStreamExpr,ctx:Context):TypedStreamExpr = tse match {
    case TypedFunApp(sf,t,ta) => TypedFunApp(apply(sf,ctx),apply(t),ta.map(apply))
    case tgt:TypedGroundTerm => apply(tgt)
  }

  def apply(tgt:TypedGroundTerm):TypedGroundTerm = tgt match {
    case TypedConst(q, t, tArgs) => TypedConst(q, apply(t), tArgs.map(apply))
    case TypedPort(p, t) => TypedPort(p,apply(t))
  }

}