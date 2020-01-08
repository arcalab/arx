package dsl.analysis.types

/**
  * Created by guillecledou on 2019-06-07
  */

/* TYPE EXPRESSIONS */

/* Type Expression */
sealed trait TExp {
  def substitute(tVar:TVar,tExp:TExp):TExp

  def vars:Set[TVar] = this match {
    case TUnit => Set()
    case t@TVar(n) => Set(t)
    case TBase(n,ps) => ps.flatMap(_.vars).toSet
    case TFun(ins,outs) => ins.vars ++ outs.vars
    case TInterface(l) => l.flatMap(_.vars).toSet
  }

  def outputs:List[TExp] = this match {
    case t@TUnit => List()
    case t@TVar(_) => List(t)
    case t@TBase(_,_) => List(t)
    case t@TInterface(l) => l
    case t@TFun(_,outs) => outs.outputs
  }
}

/* Interface Unit Type : () */
case object TUnit extends TExp {
  def substitute(tVar: TVar,tExp:TExp):TExp = this
}

/* Interface Type : T [* T] */
//case class TInterface(t1:TExp,t2:TExp) extends TExp {
//  def substitute(tVar: TVar,tExp:TExp):TExp = TInterface(t1.substitute(tVar,tExp),t2.substitute(tVar,tExp))
//}

case class TInterface(list:List[TExp]) extends TExp {
  def substitute(tVar: TVar, tExp: TExp): TInterface = TInterface(list.map(_.substitute(tVar,tExp)))
}

/* Function Type : T -> T */
case class TFun(tIn:TInterface,tOut:TInterface) extends TExp {
  def substitute(tVar: TVar,tExp:TExp):TExp = TFun(tIn.substitute(tVar,tExp),tOut.substitute(tVar,tExp))
}

/* ADT Base Type D[<A*>] */
case class TBase(name:String,tParams:List[TExp]) extends TExp {
  def substitute(tVar: TVar,tExp:TExp):TExp = TBase(name,tParams.map(_.substitute(tVar,tExp)))
}

/* Type Variable A */
case class TVar(name:String) extends TExp {
  def substitute(tVar: TVar,tExp:TExp):TExp = if (this == tVar) tExp else this

  def occurs(tExp:TExp):Boolean = tExp match {
    case TUnit => false
    case t@TVar(_) => this == t
    case TFun(ins,outs) => this.occurs(ins) || this.occurs(outs)
    case TBase(_, param) => param.exists(t=> this.occurs(t))
  }
}

/* TYPE CONSTRAINTS */

/* Type Constraint T1 = T2 */
case class TCons(l:TExp, r:TExp) {}
