package dsl.analysis.types

import dsl.backend.Simplify

/**
  * Created by guillecledou on 2019-08-05
  */


case class Substitution(sub:Map[TVar,TExp]) {
  def add(t:TVar,te: TExp) = Substitution(sub+(t->te))

  def apply(te:TExp): TExp = te match {
    case TUnit => TUnit
    case t@TVar(n) => if (sub.contains(t)) sub(t) else t
    case TFun(ins,outs) => TFun(apply(ins),apply(outs))//TFun(TInterface(ins.inputs.map(apply)),TInterface(outs.outputs.map(apply)))
    case TTensor(t1,t2) => TTensor(apply(t1),apply(t2))
    case TBase(n,ps) => TBase(n,ps.map(apply))
    case TDestr(t1) => TDestr(apply(t1))
  }

  def apply(ctx:TContext):TContext = {
    val nfuns = ctx.functions.map(f=> f._1 -> FunEntry(TFun(apply(f._2.tExp.tIn),apply(f._2.tExp.tOut)),apply(f._2.funCtx)))
    val nports:Map[String,List[PortEntry]] = ctx.ports.map(p=>p._1 -> p._2.map(pe=>PortEntry(apply(pe.tExp),pe.pType)))
    TContext(ctx.adts,nfuns,nports)
  }
}