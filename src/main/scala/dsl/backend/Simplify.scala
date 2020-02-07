package dsl.backend

import dsl.analysis.semantics.{And, Guard, GuardedCommand, True}
import dsl.analysis.types._

/**
  * Created by guillecledou on 2020-01-09
  */


object Simplify {

  def apply(tExp: TExp):TExp  = tExp match {
    case TFun(tIn, tOut) => TFun(apply(tIn),apply(tOut))
    case TTensor(t1,t2) => (apply(t1),apply(t2)) match {
      case (TUnit, TUnit) => TUnit
      case (TUnit, t3 ) => t3
      case (t3,TUnit) => t3
      case (t3,t4) => TTensor(t3,t4)
    }
    case TBase(name, tParams) => TBase(name,tParams.map(apply))
    case _ => tExp
  }

  def apply(g:Guard):Guard = g match {
    case And(g1,g2) =>(apply(g1),apply(g2)) match {
      case (True,True) => True
      case (True,g3) => g3
      case (g3,True) => g3
      case (g3,g4) => And(g3,g4)
    }
    case _ => g
  }

  def apply(gc:GuardedCommand):GuardedCommand =
    GuardedCommand(apply(gc.guard),gc.cmd)
}
