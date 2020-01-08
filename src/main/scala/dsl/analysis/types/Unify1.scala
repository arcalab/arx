package dsl.analysis.types

import dsl.common.TypeException



/**
  * Created by guillecledou on 2019-06-06
  */


object Unify1 {

  def apply(cons:Set[TCons]):Map[TVar,TExp] =
    if (cons.isEmpty) Map() else (cons.head.l, cons.head.r) match {
      case (l, r) if l == r =>
        Unify1(cons.tail)
      case (t@TVar(n), r) if !t.occurs(r) =>
        Unify1(cons.tail.map(tc => TCons(tc.l.substitute(t, r), tc.r.substitute(t, r)))) ++ Map(t -> r)
      case (l, t@TVar(n)) if !t.occurs(l) =>
        Unify1(cons.tail.map(tc => TCons(tc.l.substitute(t, l), tc.r.substitute(t, l)))) ++ Map(t -> l)
      case (TFun(i1,o1),TFun(i2,o2)) =>
        Unify1(cons.tail ++ List(TCons(i1,i2),TCons(o1,o2)))
      case (TInterface(l1),TInterface(l2)) if l1.size == l2.size =>
        Unify1(cons.tail ++ l1.zip(l2).map(p => TCons(p._1,p._2)))
      case (TBase(n1, ps1), TBase(n2, ps2)) if (ps1.size == ps2.size) && (n1 == n2) =>
        Unify1(cons.tail ++ ps1.zip(ps2).map(p => TCons(p._1, p._2)))
      case (t1,t2) => throw new TypeException(s"Impossible to unify $t1 with $t2")
    }

}