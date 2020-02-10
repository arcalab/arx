package dsl.analysis.semantics

import dsl.analysis.syntax.{Const, GroundTerm, Port}

/**
  * Created by guillecledou on 2020-02-05
  */

/**
  * A guarded command
  */
case class GuardedCommand(guard:Guard, cmd:Set[Command]) {

  def vars:Set[String] = this.outputs ++ this.inputs
  def outputs:Set[String] = cmd.map(c=>c.variable)
  def inputs:Set[String]  = guard.vars

}

/**
  * Commands (Assignments of the form x:= groundTerm)
  * @param port
  * @param term
  */
case class Command(variable:String,term:GroundTerm) {}

/**
  * Guards for commands
  */
sealed trait Guard {
  def vars:Set[String] = this match {
    case And(g1,g2) => g1.vars ++ g2.vars
    case Ask(v)     => Set(v)
    case Get(v)     => Set(v)
    case Und(v)     => Set(v)
    case True       => Set()
  }

  def &(other:Guard):Guard =
    And(this,other)

  def ->(cmd:Command):GuardedCommand =
    GuardedCommand(this,Set(cmd))
  def ->(cmd:Command*):GuardedCommand =
    GuardedCommand(this,cmd.toSet)
}

case object True extends Guard
case class And(g1:Guard,g2:Guard) extends Guard
case class Ask(variable:String) extends Guard
case class Get(variable:String) extends Guard
case class Und(variable:String) extends Guard
case class IsQ(q:String,variable:String) extends Guard
