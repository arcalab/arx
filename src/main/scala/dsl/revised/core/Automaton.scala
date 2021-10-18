package dsl.revised.core

import dsl.revised.core.{Automaton, Rule, Show, Term}
import Rule.Assignment
import dsl.revised.Error.debug
import dsl.revised.core.Term.vars

case class Automaton(init:Set[Assignment]=Set(),
                     inv:Set[Term]=Set(),
                     rs:Set[Rule]=Set(),
                     inputs:Set[String]=Set(),
                     outputs:Set[String]=Set(),
                     registers:Set[String]=Set(),
                     clocks:Set[String]=Set(),
                     args:Set[String]=Set()):

  implicit val who:String = "Aut"

  val alwaysAv = clocks++args

  /** An automaton is well defined if variables are used consistently. */
  def wellDefined: Boolean =
    (inputs++outputs).intersect(registers).isEmpty &&
      init.forall(i => registers contains i.v) &&
      inv.forall(i => vars(i).subsetOf(inputs++registers++args++clocks)) &&
      rs.forall(r =>
        r.inputs.subsetOf(inputs++registers) && // can get/ask from inputs and register
        r.outputs.subsetOf(outputs++clocks) && // can write to outputs and to clocks
        r.updated.subsetOf(registers++clocks) && // can update values of registers and clocks
        r.usedVars.subsetOf(inputs++registers++args++clocks) && //used variables must be declared (maybe this should be part of well-defined rule)
        r.get.forall(g => !(alwaysAv contains g)) && // cannot consume a clock nor an argument
        r.wellDefined(alwaysAv)
      )

  /** Composition of two automata, combining all rules */
  def *(other:Automaton):Automaton =

    val newInit = this.init++other.init
    val newInv = this.inv++other.inv
    val newIns  = this.inputs++other.inputs
    val newOuts = this.outputs++other.outputs
    val newRegs  = this.registers++other.registers
    val newClocks = this.clocks++other.clocks
    val newArgs = this.args++other.args
    var newRules = Set[Rule]()

    // adding rules that can go along
    for r <- rs       ; if r.canGoAlone(other)           do newRules += r
    for r <- other.rs ; if r.canGoAlone(otherAut = this) do newRules += r

    debug(s"- Alone: ${newRules.map(Show.apply).mkString("\n")}")

    var miss1: List[(Rule,Rule,Set[String])] = Nil
    var miss2: List[(Rule,Rule,Set[String])] = Nil

    // go through all the rules and combine the ones that yield well-formed rules
    for r1<-rs; r2<-other.rs
        if !r1.canGoAlone(other) || !r2.canGoAlone(otherAut = this) &&
           r1.vars.intersect(r2.vars).nonEmpty do // try to combine only rules that share variables
      Automaton.tryToCompose(r1,this,r2,other) match {
        case (Some(m1),_,_) => miss1 ::= m1
        case (_,Some(m2),_) => miss2 ::= m2
        case (_,_,Some(r)) => newRules += r
        case _ =>
      }

    debug(s"- Round 1: \n${miss1.map("   + "+_).mkString("\n")}\n    ---\n${miss2.map("   + "+_).mkString("\n")}")

    // for all missing rules, try to find a matching rule to be added
    while miss1.nonEmpty || miss2.nonEmpty do
      (miss1,miss2) match {
        case ((nxt1, nxt2, i1) :: _, _) =>
          for r1 <- rs if r1.inputs.intersect(i1).nonEmpty &&
                          !r1.hasLocalConflict(nxt1,this) do
            val newR1 = r1 * nxt1 // should be well-formed by construction
//            assert(newR1.wellDefined, s"Should be well defined: $r1 * $nxt1")
            if newR1.wellDefined(alwaysAv) then
              Automaton.tryToCompose(newR1, this, nxt2, other) match {
                case (Some(m1), _, _) => miss1 = (miss1.head) :: m1 :: (miss1.tail)
                case (_, Some(m2), _) => miss2 ::= m2
                case (_, _, Some(r)) => newRules += r
                case _ =>
              }
          miss1 = miss1.tail
        case (_, (nxt1, nxt2, i2) :: _) =>
          for r2 <- other.rs if r2.inputs.intersect(i2).nonEmpty &&
                                !r2.hasLocalConflict(nxt2,other) do
            val newR2 = r2 * nxt2 // should be well-formed by construction
//            assert(newR2.wellDefined, s"Should be well defined: $r2 * $nxt2")
            if newR2.wellDefined(alwaysAv) then
              Automaton.tryToCompose(nxt1, this, newR2, other) match {
                case (Some(m1), _, _) => miss1 ::= m1
                case (_, Some(m2), _) => miss2 = (miss2.head) :: m2 :: (miss2.tail)
                case (_, _, Some(r)) => newRules += r
                case _ =>
              }
          miss2 = miss2.tail
        case (_,_) =>
      }
      debug(s" - Round done. Missing: \n${miss1.map("   + "+_).mkString("\n")}\n    ---\n${miss2.map("   + "+_).mkString("\n")}")

    debug(s"===Composed===\n${Show(this)}\n  xxx\n${Show(other)}\n ===\n${Show(Automaton(newInit,newInv,newRules,newIns,newOuts,newRegs))}\n ---")
    Automaton(newInit,newInv,newRules,newIns,newOuts,newRegs,newClocks,newArgs)


  def hiding: Automaton  =
    val mixedPorts = inputs.intersect(outputs) -- registers
    val (newRs,toReplace) = rs
      .filter(_.inputs.intersect(mixedPorts).isEmpty)
      .map(_.leaveOnlyOuts(outputs--mixedPorts))
      .unzip
    var newInv = inv
    for repl <- toReplace do newInv = newInv.map(Term.keepReplacing(_,repl))
    debug(s"===Hiding===\n${Show(this)}\n  ==>\n${
      Show(Automaton(init,newInv,newRs,inputs--mixedPorts,outputs--mixedPorts,registers))}")
    Automaton(init,newInv,newRs,inputs--mixedPorts,outputs--mixedPorts,registers,clocks,args)


  override def toString: String = Show(this)


object Automaton:
  implicit val who:String = "Aut"
  private type Trp = (Rule,Rule,Set[String])
  private def tryToCompose(r1:Rule, a1:Automaton, r2:Rule, a2:Automaton): (Option[Trp],Option[Trp],Option[Rule]) =
    debug(s"=== Trying to compose: ${Show(r1)} || ${Show(r2)}")
    val i1 = r1.missingInputs(r2,a1,a2)
    if i1.nonEmpty then
      debug(s" - Missing 1: (${i1.mkString(",")})")
      (Some((r1,r2,i1)),None,None)
    else
      // case 2: r2 misses inputs
      val i2 = r2.missingInputs(r1,a2,a1)
      if i2.nonEmpty then
        debug(s" - Missing 2: (${i2.mkString(",")})")
        (None,Some((r1,r2,i2)),None)
      else
        // case 3: combined rule is well formed
        val newRule = r1*r2
        if newRule.wellDefined(a1.alwaysAv++a2.alwaysAv) then
          debug(s" - Composed: ${Show(newRule)}")
          (None,None,Some(newRule))
        else
          debug(s" - failed")
          (None,None,None)

  val empty = Automaton(Set(),Set(),Set(),Set(),Set(),Set(),Set())
  
  //// EXAMPLES
  object Examples:
    import Rule._
    import Term._
    import scala.language.implicitConversions

    implicit def str2var(s:String): Var = Var(s)

    def fifo(a:String,b:String,m:String) = Automaton(
      init=Set(), inv=Set(), rs = Set(
        get(a) & und(m) --> m /~ a,
        get(m) --> b ~~ m
      ),
      inputs = Set(a), outputs = Set(b), registers = Set(m)
    )
    def fifofull(a:String,b:String,m:String,t:Term = Term.Fun("()",Nil)) = Automaton(
      init=Set(m := t), rs = Set(
        get(a) & und(m) --> m /~ a,
        get(m) --> b ~~ m
      ),
      inputs = Set(a), outputs = Set(b), registers = Set(m)
    )
    def lossy(a:String,b:String) = Automaton(
      rs = Set(
        get(a) --> assg(b,Var(a)),
        get(a)
      ),
      inputs = Set(a), outputs = Set(b)
    )
    def drain(a:String,b:String) = Automaton(
      rs = Set(
        get(a,b)
      ),
      inputs = Set(a,b)
    )
    def xor(a:String,b:String,c:String) = Automaton(
      rs = Set(
        get(a) --> b ~~ a, // ? und(b)?
        get(a) --> c ~~ a
      ),
      inputs = Set(a), outputs = Set(b,c), registers = Set()
    )
    def dupl(a:String,b:String,c:String) = Automaton(
      init=Set(), inv=Set(), rs = Set(
        get(a) --> b ~~ a & c ~~ a
      ),
      inputs = Set(a), outputs = Set(b,c), registers = Set()
    )
    def merger(a:String,b:String,c:String) = Automaton(
      init=Set(), inv=Set(), rs = Set(
        get(a) --> c ~~ a,
        get(b) --> c ~~ b
      ),
      inputs = Set(a,b), outputs = Set(c), registers = Set()
    )
    def sync(a:String,b:String) = Automaton(
      init=Set(), inv=Set(), rs = Set(
        get(a) --> b ~~ a
      ),
      inputs = Set(a), outputs = Set(b), registers = Set()
    )
    def timer(a:String,b:String,m:String,t:String,n:Term) = Automaton(
      init=Set(),
      inv=Set(Fun("->",List(Fun("at",List(Var(m))),  Fun("<=",List(Var(t),n)) ))),
      rs = Set(
        get(a) & und(m) --> m /~ a & t /~ IntVal(0),
        get(m) & pred(Fun(">=",List(Var(t),n))) --> b ~~ m
      ),
      inputs = Set(a), outputs = Set(b), registers = Set(m), clocks = Set(t)
    )
    def timer(a:String,b:String,m:String,t:String,n:Int):Automaton =
      timer(a,b,m,t,IntVal(n))
