package dsl.analysis.semantics

import dsl.backend.Show
import dsl.common.TypeException

/**
  * Created by guillecledou on 2019-06-06
  */


object Unify {

  def apply(cons:Set[TCons]):Map[TVar,TypeExpr] =
    if (cons.isEmpty) Map() else (cons.head.l, cons.head.r) match {
      case (l, r) if l == r =>
        Unify(cons.tail)
      /* ============ beginning new case ============ */
//      /* todo: check if this makes sense here */
//      case (t@TVar(n), p@TProd(h,r))  =>
//        throw new TypeException(s"Too many arguments. Impossible to unify $t with product type ${Show(p)}")
//      case (p@TProd(h,r), t@TVar(n))  =>
//        throw new TypeException(s"Too many arguments. Impossible to unify $t with product type ${Show(p)}")
      /* ============ ending new case ============ */
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

}
