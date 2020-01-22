package dsl.analysis.types

/**
  * Created by guillerminacledou on 2020-01-21
  */


// Reactive Expression

sealed trait RExp

case object Push extends RExp
case object Pull extends RExp

case class RVar(name:String)


// Reactive Constraints

case class RCons(l:RExp,r:RExp)


