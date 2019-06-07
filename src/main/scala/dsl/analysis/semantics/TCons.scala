package dsl.analysis.semantics

/**
  * Created by guillecledou on 2019-06-07
  */

/* T1 = T2 */
case class TCons(l:TypeExpr, r:TypeExpr) {}

sealed trait TypeExpr {
  def substitute(tvar:TVar,te:TypeExpr):TypeExpr
}

/* ADT type */
case class BaseType(name:String,param:List[TypeExpr]) extends TypeExpr {
  def substitute(tvar:TVar,te:TypeExpr):BaseType = {
    BaseType(name,param.map(t => t.substitute(tvar,te)))
  }
}
/* typeExp -> typeExp */
case class TMap(from: TypeExpr, to:TypeExpr) extends TypeExpr {
  def substitute(tvar:TVar,te:TypeExpr):TMap = {
    TMap(from.substitute(tvar,te),to.substitute(tvar,te))
  }
}
/* type variable */
case class TVar(name:String) extends TypeExpr {
  def occurs(te:TypeExpr):Boolean = te match {
    case TUnit => false
    case t@TVar(n) => this == t
    case TMap(t1,t2) => this.occurs(t1) || this.occurs(t2)
    case BaseType(name, param) => param.exists(t=> this.occurs(t))
  }

  def substitute(tvar:TVar,te:TypeExpr):TypeExpr = if (this == tvar) te else this
}

case object TUnit extends TypeExpr {
  def substitute(TVar: TVar,te:TypeExpr):TypeExpr = this
}
