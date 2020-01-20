package dsl.backend

import dsl.analysis.types._

/**
  * Created by guillerminacledou on 2020-01-09
  */


object Simplify {

  def apply(tExp: TExp):TExp  = tExp
  match {
    case TFun(tIn, tOut) => TFun(apply(tIn),apply(tOut))
    case TTensor(t1,t2) => (apply(t1),apply(t2)) match {
      case (TUnit, TUnit) => TUnit
      case (TUnit, t3 ) => t3
      case (t3,TUnit) => t3
      case (t3,t4) => TTensor(t3,t4)
    }
    case TBase(name, tParams) => TBase(name,tParams.map(apply(_)))
    case _ => tExp
  }

}
