package dsl.analysis.types

/**
  * Created by guillecledou on 2019-06-07
  */


object Substitute {

  /**
    * Substitute each type variable that might appear on a type expression with the
    * concrete type expression of that variable
    * @param subst
    * @return
    */
  def apply(subst:Map[TVar,TExp]):Map[TVar,TExp] = {
    var nmap:Map[TVar,TExp] = Map()
    var known:Map[TVar,TExp] = Map()

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
  private def substitute(te: TExp, sols:Map[TVar,TExp], known:Map[TVar,TExp]):(TExp,Map[TVar,TExp]) = te match {
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
    case TBase(n,ps) =>
      // substitute each parameter of the base type remembering the new known variables
      var nk = known
      var cts = List[TExp]()
      for (p <-ps) {
        var res  = substitute(p,sols,nk)
        cts++=List(res._1)
        nk = res._2
      }
      (TBase(n,cts),nk)
    case TUnit => (TUnit,known)
    case TFun(i1,i2) =>
      // substitute i1 nad i2
      val (ci1,k1) = substitute(i1,sols,known)
      val (ci2,k2) = substitute(i2,sols,k1)
      (TFun(ci1/*.asInstanceOf[TInterface]*/,ci2/*.asInstanceOf[TInterface]*/),k2)
    case TInterface(t1,t2) =>
      // substitute t1 nad t2
      val (ct1,k1) = substitute(t1,sols,known)
      val (ct2,k2) = substitute(t2,sols,k1)
      (TInterface(ct1,ct2),k2)
  }

}