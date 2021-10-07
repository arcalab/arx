package dsl.analysis.semantics.revised

import dsl.analysis.semantics.revised.Rule.Assignment

/** A term is a Hilbert space, defined as a variable, an int (primitive type), or a function with a name a sequence of terms */
sealed trait Term

object Term:

  case class Var(v:String) extends Term: // variable
    def :=(t:Term) = Assignment(v,t)
  case class TFun(name:String,terms:List[Term]) extends Term
  case class TInt(i:Int) extends Term

  // Shortcuts to create data constructors and destructors
  def isQ(q:String,ts:List[Term]): Term = TFun(s"is§$q",ts)
  def Q(q:String,ts:List[Term]): Term = TFun(s"build§$q",ts)
  def getQ(q:String,i:Int,ts:List[Term]): Term = TFun(s"get§$q",TInt(i)::ts)

  /** Collect all ports and registers in a term */
  def vars(t:Term): Set[String] = t match {
    case Var(v) => Set(v)
    case TInt(t) => Set()
    case TFun(_,ts) => vars(ts.toSet)
  }
  /** Collect all ports and registers in a set of terms */
  def vars(ts:Set[Term]): Set[String] = ts.flatMap(vars)

  /** replace variables with terms until no more replacing is possible (error if there are loops) */
  def keepReplacing(t:Term, f:Map[String,Term],done:Set[String]=Set()): Term = t match {
    case Var(v) if done contains v =>
      sys.error(s"Found loop in assignments: '$v' repeated in ${
                f.map((k,v)=>s"$k:=${Show(v)}").mkString(", ")}.")
    case Var(v) if f contains v => keepReplacing(f(v),f,done+v)
    case TFun(n,ts) => TFun(n,ts.map(keepReplacing(_,f)))
    case _ => t // int or unknown variable
  }


