package dsl.analysis.semantics

import dsl.analysis.syntax.{Const, GroundTerm, Port}

/**
  * Created by guillecledou on 2020-02-05
  */

/**
  * A guarded command
  */
case class GuardedCommand(guard:Guard, cmd:Set[Command], highlights:Set[String]) {

  def vars:Set[String] = this.outputs ++ this.inputs

  def outputs:Set[String] = cmd.map(c=>c.variable)

  // be carful because some might be memories, given a stream builde remove the memories
  def inputs:Set[String]  = guard.variables

  // By default the set of variables with flow
  def defaultHighights: Set[String] = vars -- guard.guards.flatMap {
    case Und(v) => Set[String](v)
    case _ => Set[String]()
  }

}

/**
  * Commands (Assignments of the form x:= groundTerm)
  * @param variable
  * @param term
  */
case class Command(variable:String,term:Term) {}

/**
  * Terms in guarded commands
  */
sealed trait Term {
  def vars: Set[String] = this match {
    case Var(name) => Set(name)
    case Q(name, args) => args.toSet.flatMap((x:Term) => x.vars)
    case GetQ(name, index, term) => term.vars
  }
}

case class Var(name:String)                       extends Term
case class Q(name:String, args:List[Term])        extends Term
case class GetQ(name:String,index:Int,term:Term)  extends Term

/**
  * Guard items
  */
sealed trait GuardItem {
  //val term:String;
  def vars:Set[String] = this match {
     case Ask(v)     => Set(v)
     case Get(v)     => Set(v)
     case Und(v)     => Set() //Set(v) // todo: @Guille I think "und" is the oposite of a variable
                                       // used by the guard, at least how we use it in StreamBuilder. Changed to {}.
     case IsQ(q,t)   => t.vars
  }

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
case class Ask(v:String) extends GuardItem
case class Get(v:String) extends GuardItem
case class Und(v:String) extends GuardItem
case class IsQ(q:String, arg:Term) extends GuardItem

/**
  * Guards for commands
  * @param guards
  */
case class Guard(guards:Set[GuardItem]) {

  def variables:Set[String] =
    guards.flatMap(g=>g.vars)
  
  def &(other:Guard):Guard =
    Guard(this.guards++other.guards)

  def -->(cmd:Command):GuardedCommand =
    GuardedCommand(this,Set(cmd),defaultHhighlights(Set(cmd)))
  def -->(cmd:Iterable[Command]):GuardedCommand =
    GuardedCommand(this,cmd.toSet,defaultHhighlights(cmd.toSet))
  def -->(cmd:Command*):GuardedCommand =
    this --> cmd.toSet

  private def defaultHhighlights(cmds:Set[Command]): Set[String] =
    variables ++ cmds.map(c=>c.variable) -- guards.flatMap {
      case Und(v) => Set[String](v)
      case _ => Set[String]()
    }

}