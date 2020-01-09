package dsl.analysis.types

/**
  * Created by guillecledou on 2019-08-05
  */


case class Substitution(sub:Map[TVar,TExp]) {
  def add(t:TVar,te: TExp) = Substitution(sub+(t->te))

  def apply(te:TExp): TExp = te match {
    case TUnit => TUnit
    case t@TVar(n) => if (sub.contains(t)) sub(t) else t
    case TFun(ins,outs) => TFun(apply(ins),apply(outs))//TFun(TInterface(ins.inputs.map(apply)),TInterface(outs.outputs.map(apply)))
    case TInterface(l) => TInterface(l.map(apply))
    case TBase(n,ps) => TBase(n,ps.map(apply))
  }
}