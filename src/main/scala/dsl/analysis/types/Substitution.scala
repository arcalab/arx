package dsl.analysis.types

import dsl.analysis.syntax._
import dsl.analysis.types.TProgram.TBlock
import dsl.backend.Simplify

/**
  * Created by guillecledou on 2019-08-05
  */


case class Substitution(sub:Map[TVar,TExp]) {
  def add(t:TVar,te: TExp) = Substitution(sub+(t->te))

  def apply(te:TExp): TExp = te match {
    case TUnit => TUnit
    case t@TVar(n) => if (sub.contains(t)) sub(t) else t
    case TFun(ins, outs) => TFun(apply(ins), apply(outs))
    case TTensor(t1, t2) => TTensor(apply(t1), apply(t2))
    case TBase(n, ps) => TBase(n, ps.map(apply))
    case TDestr(t1) => TDestr(apply(t1))
  }

  def apply(te:TExp,ctx:Context): TExp = Destructor.expand(apply(te),ctx)

  def apply(ctx:Context):Context = {
    val nfuns = ctx.functions.map(f=>
      f._1 -> FunEntry(TFun(Simplify(apply(f._2.tExp.tIn,ctx)),Simplify(apply(f._2.tExp.tOut,ctx))),apply(f._2.funCtx)))
    val nports = ctx.ports.map(p=>p._1 -> p._2.map(pe=>PortEntry(apply(pe.tExp,ctx),pe.pType)))
    Context(ctx.adts,nfuns,nports)
  }

  def apply(tp:TProgram, ctx:Context):(TProgram,Context) = {
    val nctx = apply(ctx)
    (TProgram(tp.imports,tp.userTypes,apply(tp.tBlock,nctx)),nctx)
  }

  def apply(tb:TBlock, ctx:Context):TBlock = tb.map(b=>apply(b,ctx))

  def apply(ts:TStatement, ctx:Context):TStatement = ts match {
    case TFunDef(f,t,tb) => TFunDef(f,apply(t,ctx),apply(tb,ctx))
    case TSFunDef(f,t,tb) => TSFunDef(f,apply(t,ctx),apply(tb,ctx))
    case TAssignment(a,tlhs,trhs) => TAssignment(a,tlhs.map(a=>apply(a,ctx)),apply(trhs,ctx))
    case TRAssignment(a,tlhs,trhs) => TRAssignment(a,tlhs.map(a=>apply(a,ctx)),apply(trhs,ctx))
    case se:TStreamExpr => apply(se,ctx)
  }

  def apply(tsf:TStreamFun, ctx:Context):TStreamFun = tsf match {
    case TFunName(f,t,ds)  => TFunName(f,apply(t,ctx),ds.map(d=>apply(d,ctx)))
    case TBuild(tin,tout)   => TBuild(apply(tin,ctx),apply(tout,ctx))//TBuild(Destructor.expand(apply(t,ctx),ctx),Destructor.expand(apply(ta),ctx))
    case TMatch(tin,tout)   => TMatch(apply(tin,ctx),apply(tout,ctx))//TMatch(Destructor.expand(apply(t,ctx),ctx),Destructor.expand(apply(ta),ctx))
    case TSeqFun(t1,t2) => TSeqFun(apply(t1,ctx),apply(t2,ctx))
    case TParFun(t1,t2) => TParFun(apply(t1,ctx),apply(t2,ctx))
  }

  def apply(tse:TStreamExpr, ctx:Context):TStreamExpr = tse match {
    case TFunApp(sf,t,ta) => TFunApp(apply(sf,ctx),apply(t,ctx),ta.map(a=>apply(a,ctx)))
    case tgt:TGroundTerm => apply(tgt,ctx)
  }

  def apply(tgt:TGroundTerm,ctx:Context):TGroundTerm = tgt match {
    case TConst(q, t, tArgs) => TConst(q, apply(t,ctx), tArgs.map(a=>apply(a,ctx)))
    case TPort(p, t) => TPort(p,apply(t,ctx))
  }

}