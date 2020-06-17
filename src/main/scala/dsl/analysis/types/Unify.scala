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
//        println(s"[UNIFY - same] - Unified:\n-${Show(l)}\n-${Show(r)} ")
        Unify(cons.tail)
      case (t@TVar(_), r) if !t.occurs(r) =>
//        println(s"[UNIFY - tvar right] - Unified:\n-${Show(t)}\n-${Show(r)} ")
        val res = Unify(cons.tail.map(tc => TCons(tc.l.substitute(t, r), tc.r.substitute(t, r))))
        // ++ Map(t -> r)
        (res._1 ++ Map(t -> r), res._2)
      case (l, t@TVar(_)) if !t.occurs(l) =>
//        println(s"[UNIFY - tvar left] - Unified:\n-${Show(l)}\n-${Show(t)} ")
        val res = Unify(cons.tail.map(tc => TCons(tc.l.substitute(t, l), tc.r.substitute(t, l))))
        //++ Map(t -> l)
        (res._1 ++ Map(t -> l), res._2)
      case (f1@TFun(i1, o1), f2@TFun(i2, o2)) =>
//        println(s"[UNIFY - fun] - Unified:\n-${Show(f1)}\n-${Show(f2)} ")
        Unify(cons.tail ++ List(TCons(i1, i2), TCons(o1, o2)))
      case (l@TTensor(t1, t2), r@TTensor(t3, t4)) /*if t1.size == t3.size && t2.size == t4.size */ =>
//        println(s"[UNIFY - tensor] - Unified:\n-${Show(l)}\n-${Show(r)} ")
        Unify(cons.tail ++ List(TCons(t1, t3), TCons(t2, t4)))
      case (l@TBase(n1, ps1), r@TBase(n2, ps2)) if (ps1.size == ps2.size) && (n1 == n2) =>
//        println(s"[UNIFY - base] - Unified:\n-${Show(l)}\n-${Show(r)} ")
        Unify(cons.tail ++ ps1.zip(ps2).map(p => TCons(p._1, p._2)))
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

  def withReplacedDestr(cons: Set[TCons]): (Map[TVar, TExp], Set[TCons]) =
    if (cons.isEmpty) (Map(), Set()) else (cons.head.l, cons.head.r) match {
      case (TDestr(_), _) =>
        val (sol, unsol) = Unify.withReplacedDestr(cons.tail)
        (sol, unsol + cons.head)
      case (_, TDestr(_)) =>
        val (sol, unsol) = Unify.withReplacedDestr(cons.tail)
        (sol, unsol + cons.head)
      case (l, r) if l == r =>
        //        println(s"[UNIFY - same] - Unified:\n-${Show(l)}\n-${Show(r)} ")
        Unify.withReplacedDestr(cons.tail)
      case (t@TVar(_), r) if !t.occurs(r) && !r.isInstanceOf[TTensor] && !r.isInstanceOf[TFun] =>
        //        println(s"[UNIFY - tvar right] - Unified:\n-${Show(t)}\n-${Show(r)} ")
        val res = Unify.withReplacedDestr(cons.tail.map(tc => TCons(tc.l.substitute(t, r), tc.r.substitute(t, r))))
        // ++ Map(t -> r)
        (res._1 ++ Map(t -> r), res._2)
      case (l, t@TVar(_)) if !t.occurs(l) && !l.isInstanceOf[TTensor] && !l.isInstanceOf[TFun] =>
        //        println(s"[UNIFY - tvar left] - Unified:\n-${Show(l)}\n-${Show(t)} ")
        val res = Unify.withReplacedDestr(cons.tail.map(tc => TCons(tc.l.substitute(t, l), tc.r.substitute(t, l))))
        //++ Map(t -> l)
        (res._1 ++ Map(t -> l), res._2)
      case (f1@TFun(i1, o1), f2@TFun(i2, o2)) =>
        //        println(s"[UNIFY - fun] - Unified:\n-${Show(f1)}\n-${Show(f2)} ")
        Unify.withReplacedDestr(cons.tail ++ List(TCons(i1, i2), TCons(o1, o2)))
      case (l@TTensor(t1, t2), r@TTensor(t3, t4)) /*if t1.size == t3.size && t2.size == t4.size */ =>
        //        println(s"[UNIFY - tensor] - Unified:\n-${Show(l)}\n-${Show(r)} ")
        Unify.withReplacedDestr(cons.tail ++ List(TCons(t1, t3), TCons(t2, t4)))
      case (l@TBase(n1, ps1), r@TBase(n2, ps2)) if (ps1.size == ps2.size) && (n1 == n2) =>
        //        println(s"[UNIFY - base] - Unified:\n-${Show(l)}\n-${Show(r)} ")
        Unify.withReplacedDestr(cons.tail ++ ps1.zip(ps2).map(p => TCons(p._1, p._2)))
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
