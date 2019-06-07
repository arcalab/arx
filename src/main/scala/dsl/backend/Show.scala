package dsl.backend

import dsl.analysis.semantics._


/**
  * Created by guillecledou on 2019-06-07
  */


object Show {
  def apply(te:TypeExpr):String = te match {
    case TVar(n) => s"T$n"
    case TMap(f, t) => Show(f) +" -> " + Show(t)
    case BaseType(n,Nil) => n
    case BaseType(n, ps) => n + ps.map(Show(_)).mkString("<",",",">")
    case TUnit => "()"
  }

}
