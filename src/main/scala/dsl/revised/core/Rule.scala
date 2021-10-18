package dsl.revised.core

import Rule.Assignment
import Term.Var

case class Rule(get:Set[String], ask:Set[String], und:Set[String], pred:Set[Term],
                assg:Set[Assignment], upd:Set[Assignment], highlights:Set[String]):

  /** ports and registers that are read, ignoring mixed ports */
  def inputs: Set[String] = get++ask
  /** inputs + mixed ports of the automaton that are used */
  def allInputs(a:Automaton): Set[String] = inputs++(outputs.intersect(a.inputs))
  /** ports and registers that are written in the current round */
  def outputs: Set[String] = assg.map(_.v)
  /** registers that are updated for the next round*/
  def updated: Set[String] = upd.map(_.v)
  /** ports and registers that are read and written in the current round */
  def vars: Set[String] = inputs++outputs
  /** terms that are used, including predicates */
  def used: Set[Term] = pred ++ assg.map(_.t) ++ upd.map(_.t)
  /** ports and registers in terms that are used, including predicates */
  def usedVars: Set[String] = Term.vars(used)

  implicit val who:String = "Rule"
  import dsl.revised.Error.debug

  /** A rule is well defined if get/ask/und are disjoint, used variables are aligned with get/ask/und,
    * and there are no multiple assignments to the same variable */
  def wellDefined(alwaysAv:Set[String]): Boolean =
    get.intersect(ask).isEmpty &&
      ask.intersect(und).isEmpty &&
      und.intersect(get).isEmpty &&
      usedVars.subsetOf(vars++alwaysAv) && // extra variables can be used
      usedVars.intersect(und).isEmpty &&
      outputs.intersect(und).isEmpty &&
      alwaysAv.intersect(und).isEmpty && // extra variables cannot be undefined
      outputs.size == assg.size &&
      updated.size == upd.size
      // maybe forbid to assign in the current round a register (only for the next)

  /** A port can go alone if it does not use an input variable of another automaton */
  def canGoAlone(otherAut:Automaton) =
    debug(s"[ vars $vars # ${otherAut.inputs} ]")
    (vars intersect otherAut.inputs).isEmpty

  /** Two rules from the same automata have no conflicts if they use disjoint ports+registers */
  def hasLocalConflict(other:Rule,a:Automaton): Boolean =
    val res =
      allInputs(a).intersect(other.allInputs(a)).nonEmpty ||
      outputs.intersect(other.outputs).nonEmpty ||
      updated.intersect(other.updated).nonEmpty
    if !res then debug(s"no conflict for $this VS $other")
    res

  // checks which inputs are missing to be composed with an `other` rule.
  def missingInputs(other:Rule, thisAut:Automaton, otherAut:Automaton): Set[String] =
    debug(s"''' other inputs: ${other.inputs.mkString(",")}, autIn: ${(thisAut.inputs++otherAut.inputs).mkString(",")}, myVars: ${vars.mkString(",")}.")
    (other.outputs.intersect(thisAut.inputs++(otherAut.inputs--otherAut.outputs)) -- inputs) ++
    (other.inputs .intersect(thisAut.inputs /* ++otherAut.inputs */) -- vars)

  // b->c can go with other a->b? Missing inputs:
  //  - other "b" inters. (all inputs "a,b") minus this "b" ++  []
  //    other "a" inters. (all inputs "a,b") minus this "b,c"   [a] ==
  ///FIX other "a" inters. (MY inputs "b") minus my vars "b,c"

  // get(a,b) can go with other a->x->v, b->m3'
  // FIX other "x,v" inter (MY inputs "a,b" and other MISSING INPUTS "a,b") minus this ins "a,b" ++  [x]
  //     other "a,b,x" inter (MY inputs "a,b") minus this vars "a,b"     []

  // a->b can go with other a->c
  //     other "c" inter (MY ins "a" and OTHER ins+ "a") minus MY "a" ++  []
  //     other "a" inter (MY ins "a") minus MY vars "a,b"                 []

  // composes two rules
  def *(other:Rule):Rule =
    val toHide = outputs ++ other.outputs
    // concatenate all and remove outputs from get.ask
    Rule((get++other.get)--toHide, (ask++other.ask)--toHide, und++other.und, pred++other.pred,
      assg++other.assg, upd++other.upd, highlights++other.highlights)

  def leaveOnlyOuts(vars:Set[String]): (Rule,Map[String,Term]) =
    val (okAssg,oldAssg) = assg.partition(vars contains _.v)
    val toReplace = oldAssg.map(x => x.v -> x.t).toMap
    val newPred = pred.map(Term.keepReplacing(_,toReplace))
    val newAssg = assg.filterNot(toReplace contains _.v)
                      .map(a => Assignment(a.v , Term.keepReplacing(a.t,toReplace)))
    val newUpd =  upd .map(a => Assignment(a.v , Term.keepReplacing(a.t,toReplace)))
    val newUnd = und.filterNot(toReplace.contains) // in well-defined rules und does not appear in terms.
    (Rule(get,ask,newUnd,newPred,newAssg,newUpd,highlights),toReplace)

  // constructor helpers
  def &(r:Rule): Rule =
    Rule(get++r.get,ask++r.ask,und++r.und,pred++r.pred,
          assg++r.assg,upd++r.upd,highlights++r.highlights)
  val --> = &

  override def toString: String = Show(this)

object Rule:

  case class Assignment(v:String, t:Term)

  // constructor helpers
  def get(s:String*) = Rule(s.toSet,Set(),Set(),Set(),Set(),Set(),Set())
  def ask(s:String*) = Rule(Set(),s.toSet,Set(),Set(),Set(),Set(),Set())
  def und(s:String*) = Rule(Set(),Set(),s.toSet,Set(),Set(),Set(),Set())
  def pred(ts:Term*) = Rule(Set(),Set(),Set(),ts.toSet,Set(),Set(),Set())
  def assg(v:String,t:Term) = Rule(Set(),Set(),Set(),Set(),Set(Assignment(v,t)),Set(),Set())
  def upd(v:String,t:Term) = Rule(Set(),Set(),Set(),Set(),Set(),Set(Assignment(v,t)),Set())
