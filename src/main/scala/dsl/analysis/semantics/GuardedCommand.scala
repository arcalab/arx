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
  
  // be carful because some might be memories, given a stream builde remove the memories
  def inputs:Set[String]  = guard.variables 
}

/**
  * Commands (Assignments of the form x:= groundTerm)
  * @param port
  * @param term
  */
case class Command(variable:String,term:Term) {}

/**
  * Terms in guarded commands  
  */ 
sealed trait Term 

case class Var(name:String)                       extends Term
case class Q(name:String, args:List[Term])        extends Term
case class GetQ(name:String,index:Int,term:Term)  extends Term

/**
  * Guard items
  */
sealed trait GuardItem {
  val variable:String; 
  // def vars:Set[String] = this match {
  //   case And(g1,g2) => g1.vars ++ g2.vars
  //   case Ask(v)     => Set(v)
  //   case Get(v)     => Set(v)
  //   case Und(v)     => Set(v)
  //   case IsQ(q,v)   => Set(v)
  //   case True       => Set()
  // }

  def &(other:GuardItem):Guard =
    Guard(Set(this,other))

  def &(other:Guard):Guard =
    Guard(other.guards+this)

  // def ->(cmd:Command):GuardedCommand =
  //   GuardedCommand(this,Set(cmd))
  // def ->(cmd:Command*):GuardedCommand =
  //   GuardedCommand(this,cmd.toSet)
}

//case object True extends GuardItem
//case class And(g1:GuardItem,g2:GuardItem) extends GuardItem
case class Ask(variable:String) extends GuardItem
case class Get(variable:String) extends GuardItem
case class Und(variable:String) extends GuardItem
case class IsQ(q:String,variable:String) extends GuardItem

/**
  * Guards for commands
  * @param guards
  */
case class Guard(guards:Set[GuardItem]) {

  def variables:Set[String] = 
    guards.map(g=>g.variable)
  
  def &(other:Guard):Guard =
    Guard(this.guards++other.guards)

  def ->(cmd:Command):GuardedCommand =
    GuardedCommand(this,Set(cmd))
  def ->(cmd:Command*):GuardedCommand =
    GuardedCommand(this,cmd.toSet)  
}