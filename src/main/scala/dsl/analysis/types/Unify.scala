package dsl.analysis.types

import dsl.backend.Show
import dsl.common.TypeException



/**
  * Created by guillecledou on 2019-06-06
  */


object Unify {

  /**
    * (New) Unify type constraints
    *
    * @param cons set of type constraints
    * @return a map from variables to type expressions they represent,
    *         and a set of unsolved constraints for destructors types.
    */
  def apply(cons: Set[TCons]): (Map[TVar, TExp], Set[TCons]) =
    if (cons.isEmpty) (Map(), Set()) else (cons.head.l, cons.head.r) match {
      case (TDestr(_), _) =>
        val (sol, unsol) = Unify(cons.tail)
        (sol, unsol + cons.head)
      case (_, TDestr(_)) =>
        val (sol, unsol) = Unify(cons.tail)
        (sol, unsol + cons.head)
      case (l, r) if l == r =>
        Unify(cons.tail)
      case (t@TVar(n), r) if !t.occurs(r) =>
        val res = Unify(cons.tail.map(tc => TCons(tc.l.substitute(t, r), tc.r.substitute(t, r))))
        // ++ Map(t -> r)
        (res._1 ++ Map(t -> r), res._2)
      case (l, t@TVar(n)) if !t.occurs(l) =>
        val res = Unify(cons.tail.map(tc => TCons(tc.l.substitute(t, l), tc.r.substitute(t, l))))
        //++ Map(t -> l)
        (res._1 ++ Map(t -> l), res._2)
      case (TFun(i1, o1), TFun(i2, o2)) =>
        Unify(cons.tail ++ List(TCons(i1, i2), TCons(o1, o2)))
      case (TTensor(t1, t2), TTensor(t3, t4)) /*if t1.size == t3.size && t2.size == t4.size */ =>
        Unify(cons.tail ++ List(TCons(t1, t3), TCons(t2, t4)))
      case (TBase(n1, ps1), TBase(n2, ps2)) if (ps1.size == ps2.size) && (n1 == n2) =>
        Unify(cons.tail ++ ps1.zip(ps2).map(p => TCons(p._1, p._2)))
      // new, unify destructors
      //      case (TDestr(t1),TDestr(t2)) =>
      //        val (sol,unsol) = Unify(cons.tail ++ List(TCons(t1,t2)))
      //        (sol,unsol + cons.head)
      //      //case (TDestr(t1),t2@TBase(n1,ps1)) =>
      //      //  Unify(cons.tail ++ List(TCons(t1,t2)))
      //      case (TDestr(t1),TTensor(t2,t3)) =>
      //        val res = Unify(cons.tail)
      //        (res._1,res._2 + cons.head)
      //      case (TTensor(t2,t3),TDestr(t1)) =>
      //        val res = Unify(cons.tail)
      //        (res._1,res._2 + cons.head)
      case (t1, t2) => throw new TypeException(s"Impossible to unify ${Show(t1)} with ${Show(t2)}")
    }
}
