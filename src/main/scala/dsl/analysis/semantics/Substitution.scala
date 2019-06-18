package dsl.analysis.semantics

/**
  * Created by guillerminacledou on 2019-06-07
  */


object Substitution {

  /**
    * Substitute each type variable that might appear on a type expression with the
    * concrete type expression of that variable
    * @param subst
    * @return
    */
  def apply(subst:Map[TVar,TypeExpr]):Map[TVar,TypeExpr] = {
    var nmap:Map[TVar,TypeExpr] = Map()
    var known:Map[TVar,TypeExpr] = Map()

    for((tv,te) <-subst) {
      var res = substitute(te, subst, known)
      known = res._2
      nmap+= tv -> res._1
    }
    nmap
  }

  /**
    * Given a type expression substitute any type variable it might have
    * with the concrete type expression of the variable
    * @param te
    * @param sols
    * @param known
    * @return
    */
  private def substitute(te: TypeExpr, sols:Map[TVar,TypeExpr],known:Map[TVar,TypeExpr]):(TypeExpr,Map[TVar,TypeExpr]) = te match {
    case t@TVar(n) =>
      // if concrete type is already known return it
      if (known.contains(t))
        (known(t),known)
      else {
        // find the concrete type of t and add t to the known variables
        if (sols.contains(t)) {
          val (ct, nk) = substitute(sols(t), sols, known)
          (ct, nk + (t -> ct))
        } else
          // todo: if t is not defined in the solution, let it undefined for know
          (t,known)
      }
    case BaseType(n,ps) =>
      // substitute each parameter of the base type remembering the new known variables
      var nk = known
      var cts = List[TypeExpr]()
      for (p <-ps) {
        var res  = substitute(p,sols,nk)
        cts++=List(res._1)
        nk = res._2
      }
      (BaseType(n,cts),nk) //BaseType(n,ps.map(p => substitute(p,sols)))
    case TMap(t1,t2) =>
      // substitute each sub type expression
      val (ct1,k1) = substitute(t1,sols,known)
      val (ct2,k2) = substitute(t2,sols,k1)
      (TMap(ct1,ct2),k2)
    case TUnit => (TUnit,known)
  }
}
