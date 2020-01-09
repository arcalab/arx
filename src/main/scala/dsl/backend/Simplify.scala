package dsl.backend

import dsl.analysis.types._

/**
  * Created by guillerminacledou on 2020-01-09
  */


object Simplify {

  def apply(tExp: TExp):TExp  = tExp
  match {
    case TFun(tIn, tOut) => TFun(apply(tIn),apply(tOut))
    case TInterface(Nil) => TUnit
    case TInterface(list) =>
      val nl:List[TExp] = list.map(apply(_)).filterNot(p => p==TUnit)
      if (nl.isEmpty) TUnit else TInterface(nl)
    case TBase(name, tParams) => TBase(name,tParams.map(apply(_)))
    case _ => tExp
  }

}
