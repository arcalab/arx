package dsl.analysis.types

import dsl.common.TypeException



/**
  * Created by guillecledou on 2019-06-06
  */


object Unify {

  def apply(cons:Set[TCons]):Map[TVar,TExp] =
    if (cons.isEmpty) Map() else (cons.head.l, cons.head.r) match {
      case (l, r) if l == r =>
        Unify(cons.tail)
      case (t@TVar(n), r) if !t.occurs(r) =>
        Unify(cons.tail.map(tc => TCons(tc.l.substitute(t, r), tc.r.substitute(t, r)))) ++ Map(t -> r)
//        Unify1(cons.tail) ++ Map(t -> r)
      case (l, t@TVar(n)) if !t.occurs(l) =>
        Unify(cons.tail.map(tc => TCons(tc.l.substitute(t, l), tc.r.substitute(t, l)))) ++ Map(t -> l)
//        Unify1(cons.tail) ++ Map(t -> l)
      case (TFun(i1,o1),TFun(i2,o2)) =>
        Unify(cons.tail ++ List(TCons(i1,i2),TCons(o1,o2)))
      case (TTensor(t1,t2),TTensor(t3,t4)) /*if t1.size == t3.size && t2.size == t4.size */=>
        Unify(cons.tail ++ List(TCons(t1,t3),TCons(t2,t4)))
      case (TBase(n1, ps1), TBase(n2, ps2)) if (ps1.size == ps2.size) && (n1 == n2) =>
        Unify(cons.tail ++ ps1.zip(ps2).map(p => TCons(p._1, p._2)))
      // new, unify destructors
      case (TDestr(t1),TDestr(t2)) =>
        Unify(cons.tail ++ List(TCons(t1,t2)))
      case (TDestr(t1),t2@TBase(n1,ps1)) => //TODO: check if it makes sense
        Unify(cons.tail ++ List(TCons(t1,t2)))
//      case (TDestr(t1),TTensor(t2,t3)) =>
//        Unify(cons.tail)
//      case (TTensor(t2,t3),TDestr(t1)) =>
//        Unify(cons.tail)
      case (t1,t2) => throw new TypeException(s"Impossible to unify $t1 with $t2")
    }


}