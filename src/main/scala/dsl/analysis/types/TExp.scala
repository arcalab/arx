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
    case TTensor(t1,t2) => t1.vars ++ t2.vars
  }

  def outputs:List[TExp] = this match {
    case t@TUnit => List()
    case t@TVar(_) => List(t)
    case t@TBase(_,_) => List(t)
    case t@TTensor(t1,t2) => t1.outputs ++ t2.outputs
    case t@TFun(_,outs) => outs.outputs
    case TDestr(t1) => t1.outputs
//    case t@TTProj(t1, index) => List(t)
  }

  def inputs:List[TExp] = this match {
    case t@TUnit => List()
    case t@TVar(_) => List(t)
    case t@TBase(_,_) => List(t)
    case TTensor(t1,t2) => t1.inputs ++ t2.inputs
    case TFun(ins,_) => ins.inputs
    case TDestr(t1) => t1.inputs
//    case t@TTProj(t1, index) => List(t)
  }

}

/* Interface Unit Type : () */
case object TUnit extends TExp {
  def substitute(tVar: TVar,tExp:TExp):TExp = this
}

/* Tensor Type : T * T */
case class TTensor(t1:TExp, t2:TExp) extends TExp {
  def substitute(tVar: TVar,tExp:TExp):TExp = TTensor(t1.substitute(tVar,tExp),t2.substitute(tVar,tExp))

}

//case class TInterface(list:List[TExp]) extends TExp {
//  def substitute(tVar: TVar, tExp: TExp): TInterface = TInterface(list.map(_.substitute(tVar,tExp)))
//}

/* Function Type : T -> T */
case class TFun(tIn:TExp,tOut:TExp) extends TExp {
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

/* Type Destructor */
case class TDestr(t:TExp) extends TExp {
  def substitute(tVar: TVar, tExp: TExp): TExp = TDestr(t.substitute(tVar,tExp))
}

///* Type Term Projection */
//case class TTProj(t:TDestr,index:Int) extends TExp {
//  def substitute(tVar: TVar, tExp: TExp): TExp = TTProj(TDestr(t.t.substitute(tVar,tExp)),index)
//}

/* TYPE CONSTRAINTS */

/* Type Constraint T1 = T2 */
case class TCons(l:TExp, r:TExp) {}
