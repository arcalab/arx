package dsl

import common.TypeException


/**
  * Created by guillecledou on 2019-06-06
  */


object Unify {

  def apply(cons:Set[TCons]):Map[TVar,TypeExpr] =
    if (cons.isEmpty) Map() else (cons.head.l, cons.head.r) match {
      case (l, r) if l == r =>
        Unify(cons.tail)
      case (t@TVar(n), r) if !t.occurs(r) =>
        Unify(cons.tail.map(tc => TCons(tc.l.substitute(t, r), tc.r.substitute(t, r)))) ++ Map(t -> r)
      case (l, t@TVar(n)) if !t.occurs(l) =>
        Unify(cons.tail.map(tc => TCons(tc.l.substitute(t, l), tc.r.substitute(t, l)))) ++ Map(t -> l)
      case (TMap(t1, t2), TMap(t3, t4)) =>
        Unify(cons.tail ++ List(TCons(t1, t3), TCons(t2, t4)))
      case (BaseType(n1, ps1), BaseType(n2, ps2)) if (ps1.size == ps2.size) && (n1 == n2) =>
        Unify(cons.tail ++ ps1.zip(ps2).map(p => TCons(p._1, p._2)))
      case (t1,t2) => throw new TypeException(s"Impossible to unify $t1 with $t2")
  }

  def simplify(sol:Map[TVar,TypeExpr]):Map[TVar,TypeExpr] = {
    var sols = sol.toSet
    var res = sols.map(s => simplify(s,sols))
    res.toMap
  }

  private def simplify(s:(TVar,TypeExpr),sols:Set[(TVar,TypeExpr)]):(TVar,TypeExpr) = s match {
    case (t1@TVar(n1),t2@TVar(n2)) =>
      // find the definition of t2
      var te = sols.find(sub => sub._1==t2)
      // if it exist return a new substitution from t1 to the actual type expression of t2
      if (te.isDefined) (t1,te.get._2) else  (t1,t2)
    case _ => s
  }

}
