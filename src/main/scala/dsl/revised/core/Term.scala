package dsl.revised.core

import dsl.revised.core.Rule.Assignment
import dsl.revised.core.{Show, Term}

/** A term is a Hilbert space, defined as a variable, an int (primitive type), or a function with a name a sequence of terms */
sealed trait Term

object Term:

  case class Var(v:String) extends Term: // variable
    def :=(t:Term) = Assignment(v,t)
    def ~~(t:Term) = Rule.assg(v,t)
    def ~~(a:String) = Rule.assg(v,Var(a))
    def /~(t:Term) = Rule.upd(v,t)
    def /~(a:String) = Rule.upd(v,Var(a))
  case class Fun(name:String,terms:List[Term]) extends Term
  case class IntVal(i:Int) extends Term

  // Shortcuts to create data constructors and destructors
  def isQ(q:String,ts:List[Term]): Term = Fun(s"is§$q",ts)
  def Q(q:String,ts:List[Term]): Term = Fun(s"build§$q",ts)
  def getQ(q:String,i:Int,ts:List[Term]): Term = Fun(s"get§$q",IntVal(i)::ts)
  val trueT: Term = Fun("True",Nil)
  val falseT: Term = Fun("False",Nil)
  val unitT: Term = Fun("()",Nil)

  /** Collect all ports and registers in a term */
  def vars(t:Term): Set[String] = t match {
    case Var(v) => Set(v)
    case IntVal(t) => Set()
    case Fun(_,ts) => vars(ts.toSet)
  }
  /** Collect all ports and registers in a set of terms */
  def vars(ts:Set[Term]): Set[String] = ts.flatMap(vars)

  /** replace variables with terms until no more replacing is possible (error if there are loops) */
  def keepReplacing(t:Term, f:Map[String,Term],done:Set[String]=Set()): Term = t match {
    case Var(v) if done contains v =>
      sys.error(s"Found loop in assignments: '$v' repeated in ${
                f.map((k,v)=>s"$k:=${Show(v)}").mkString(", ")}.")
    case Var(v) if f contains v => keepReplacing(f(v),f,done+v)
    case Fun(n,ts) => Fun(n,ts.map(keepReplacing(_,f)))
    case _ => t // int or unknown variable
  }

  //def isGrounded(t:Term) = vars(t).isEmpty
  
  /////////////////////
  // Interpretations //
  /////////////////////

  // move to different file?
  type Interpretation = PartialFunction[List[Term],Term]
  
  /** Bottom-up application of an interpretation. */
  def evaluate(t:Term)(using is: Map[String,Interpretation]): Term = t match
    case Fun(name,ts) if is contains name =>
      val ts2 = ts.map(evaluate)
      is(name).applyOrElse(ts2,_ => Fun(name,ts2))
    case _ => t
  



