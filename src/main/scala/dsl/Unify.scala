package dsl


/**
  * Created by guillecledou on 2019-06-06
  */


object Unify {

  def apply(cons:Set[TCons]):Map[TVar,TypeExpr] = {
    if (cons.isEmpty) {println("no more constraints"); Map()}
    else {
      var c = cons.head
      (c.l,c.r) match {
        case (l,r) if l==r =>
          println(s"T1 = T2: $l = $r ")
          println("to unify: "+ cons.tail)
          Unify(cons.tail)
        case (t@TVar(n),r) if !(t.occurs(r)) =>
          println(s"TVAR -> T2: $t -> $r ")
          var toUn = cons.tail.map(tc => TCons(tc.l.substitute(t,r),tc.r.substitute(t,r)))
          println("to unify: "+ toUn)
          Unify(toUn) ++ Map(t->r)
        case (l,t@TVar(n)) if !(t.occurs(l)) =>
          println(s"TVAR -> T1: $t -> $l ")
          var toUn = cons.tail.map(tc => TCons(tc.l.substitute(t,l),tc.r.substitute(t,l)))
          println("to unify: "+ toUn)
          Unify(toUn) ++ Map(t->l)
        case (TMap(t1,t2),TMap(t3,t4)) =>
          println(s"T1 -> T2, T3 -> T4:\n $t1 = $t3 and \n $t2 = $t4")
          var toUn = cons.tail++ List(TCons(t1,t3),TCons(t2,t4))
          println("to unify: "+ toUn)
          Unify(toUn)
        case (BaseType(n1,ps1),BaseType(n2,ps2)) if (ps1.size == ps2.size) && (n1==n2) =>
          var toUn = cons.tail++ ps1.zip(ps2).map(p => TCons(p._1,p._2))
          println("to unify: "+ toUn)
          Unify(toUn)
      }
    }

  }


}
