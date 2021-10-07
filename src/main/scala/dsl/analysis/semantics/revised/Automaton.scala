package dsl.analysis.semantics.revised

import dsl.analysis.semantics.revised.Rule.Assignment

case class Automaton(init:Set[Assignment], rs:Set[Rule]
                     , inputs:Set[String]=Set(), outputs:Set[String]=Set(), registers:Set[String]=Set()):

  def wellDefined: Boolean =
    (inputs++outputs).intersect(registers).isEmpty &&
      init.forall(i => registers contains i.v) &&
      rs.forall(r =>
        r.inputs.subsetOf(inputs++registers) &&
        r.outputs.subsetOf(outputs) &&
        r.updated.subsetOf(registers) &&
        r.usedVars.subsetOf(inputs++registers) && //maybe this should be part of well-defined rule
        r.wellDefined
      )

  /** Composition of two automata, combining all rules */
  def *(other:Automaton):Automaton =

    val newInit = this.init++other.init
    val newIns  = this.inputs++other.inputs
    val newOuts = this.outputs++other.outputs
    val newRegs  = this.registers++other.registers
    var newRules = Set[Rule]()

    // adding rules that can go along
    for r <- rs       ; if r.canGoAlone(other)           do newRules += r
    for r <- other.rs ; if r.canGoAlone(otherAut = this) do newRules += r

    println(s"- Alone: ${newRules.map(Show.apply).mkString("\n")}")

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

    println(s"- Round 1: \n${miss1.map("   + "+_).mkString("\n")}\n    ---\n${miss2.map("   + "+_).mkString("\n")}")

    // for all missing rules, try to find a matching rule to be added
    while miss1.nonEmpty || miss2.nonEmpty do
      (miss1,miss2) match {
        case ((nxt1, nxt2, i1) :: _, _) =>
          for r1 <- rs if r1.inputs.intersect(i1).nonEmpty &&
                          !r1.hasLocalConflict(nxt1,this) do
            val newR1 = r1 * nxt1 // should be well-formed by construction
//            assert(newR1.wellDefined, s"Should be well defined: $r1 * $nxt1")
            if newR1.wellDefined then
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
            if newR2.wellDefined then
              Automaton.tryToCompose(nxt1, this, newR2, other) match {
                case (Some(m1), _, _) => miss1 ::= m1
                case (_, Some(m2), _) => miss2 = (miss2.head) :: m2 :: (miss2.tail)
                case (_, _, Some(r)) => newRules += r
                case _ =>
              }
          miss2 = miss2.tail
        case (_,_) =>
      }
      println(s" - Round done. Missing: \n${miss1.map("   + "+_).mkString("\n")}\n    ---\n${miss2.map("   + "+_).mkString("\n")}")

    println(s"===Composed===\n${Show(this)}\n  xxx\n${Show(other)}\n ===\n${Show(Automaton(newInit,newRules,newIns,newOuts,newRegs))}\n ---")
    Automaton(newInit,newRules,newIns,newOuts,newRegs)


  def hiding: Automaton  =
    val mixedPorts = inputs.intersect(outputs) -- registers
    val newRs = rs
      .filter(_.inputs.intersect(mixedPorts).isEmpty)
      .map(_.leaveOnlyOuts(outputs--mixedPorts))
    println(s"===Hiding===\n${Show(this)}\n  ==>\n${Show(Automaton(init,newRs,inputs--mixedPorts,outputs--mixedPorts,registers))}")
    Automaton(init,newRs,inputs--mixedPorts,outputs--mixedPorts,registers)



  override def toString: String = Show(this)


object Automaton:
  private type Trp = (Rule,Rule,Set[String])
  private def tryToCompose(r1:Rule, a1:Automaton, r2:Rule, a2:Automaton): (Option[Trp],Option[Trp],Option[Rule]) =
    println(s"=== Trying to compose: ${Show(r1)} || ${Show(r2)}")
    val i1 = r1.missingInputs(r2,a1,a2)
    if i1.nonEmpty then
      println(s" - Missing 1: (${i1.mkString(",")})")
      (Some((r1,r2,i1)),None,None)
    else
      // case 2: r2 misses inputs
      val i2 = r2.missingInputs(r1,a2,a1)
      if i2.nonEmpty then
        println(s" - Missing 2: (${i2.mkString(",")})")
        (None,Some((r1,r2,i2)),None)
      else
        // case 3: combined rule is well formed
        val newRule = r1*r2
        if newRule.wellDefined then
          println(s" - Composed: ${Show(newRule)}")
          (None,None,Some(newRule))
        else
          println(s" - failed")
          (None,None,None)

  val empty = Automaton(Set(),Set(),Set(),Set(),Set())

  //// EXAMPLES

  import Rule._
  import Term._

  def fifo(a:String,b:String,m:String) = Automaton(
    init=Set(), rs = Set(
      get(a) & und(m) --> upd(m,Var(a)),
      get(m) --> assg(b,Var(m))
    ),
    inputs = Set(a), outputs = Set(b), registers = Set(m)
  )
  def fifofull(a:String,b:String,m:String) = Automaton(
    init=Set(Assignment(m,TFun("",Nil))), rs = Set(
      get(a) & und(m) --> upd(m,Var(a)),
      get(m) --> assg(b,Var(m))
    ),
    inputs = Set(a), outputs = Set(b), registers = Set(m)
  )
  def lossy(a:String,b:String) = Automaton(
    init=Set(), rs = Set(
      get(a) --> assg(b,Var(a)),
      get(a)
    ),
    inputs = Set(a), outputs = Set(b), registers = Set()
  )
  def drain(a:String,b:String) = Automaton(
    init=Set(), rs = Set(
      get(a,b)
    ),
    inputs = Set(a,b), outputs = Set(), registers = Set()
  )
  def xor(a:String,b:String,c:String) = Automaton(
    init=Set(), rs = Set(
      get(a) --> assg(b,Var(a)), // ? und(b)?
      get(a) --> assg(c,Var(a))
    ),
    inputs = Set(a), outputs = Set(b,c), registers = Set()
  )
  def dupl(a:String,b:String,c:String) = Automaton(
    init=Set(), rs = Set(
      get(a) --> assg(b,Var(a)) & assg(c,Var(a))
    ),
    inputs = Set(a), outputs = Set(b,c), registers = Set()
  )
  def merger(a:String,b:String,c:String) = Automaton(
    init=Set(), rs = Set(
      get(a) --> assg(c,Var(a)),
      get(b) --> assg(c,Var(b))
    ),
    inputs = Set(a,b), outputs = Set(c), registers = Set()
  )
  def sync(a:String,b:String) = Automaton(
    init=Set(), rs = Set(
      get(a) --> assg(b,Var(a))
    ),
    inputs = Set(a), outputs = Set(b), registers = Set()
  )