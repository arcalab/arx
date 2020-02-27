package dsl.analysis.semantics

import dsl.backend.Show
import dsl.common.TimeoutException

/**
  * Automaton built from a given [[StreamBuilder]]
  * @param sts set of states
  * @param init initial state
  * @param trans set of transitions
  * @param sb stream builder
  */
case class SBAutomata(sts:Set[SBState],init:SBState,trans:Set[SBTrans],sb:StreamBuilder) {}

/**
  * State of a [[SBAutomata]]
  * @param memories set of defined memory variables
  * @param activeIns set of active input streams
  * @param activeOuts set of active output streams
  */
case class SBState(memories:Set[String],activeIns:Set[String],activeOuts:Set[String]) {}

/**
  * Transition of a [[SBAutomata]]
  */
sealed trait SBTrans {
  val from,to:SBState;

  def label:String = this match {
    case UpdTrans(from, gc, ins, outs, to) =>
      val getvars = gc.guard.guards.collect({case g:Get=>g}).map(_.variable).mkString(",")
      val askvars = gc.guard.guards.collect({case g:Ask=>g}).map(_.variable).mkString(",")
      val undvars = gc.guard.guards.collect({case g:Und=>g}).map(_.variable).mkString(",")
      val isqs = gc.guard.guards.collect({case g:IsQ=>g}).map(v=>s"is${v.q}${v.variable}").mkString(",")

      val gets = if (getvars.isEmpty) "" else s"Get($getvars)"
      val asks = if (askvars.isEmpty) "" else s"Ask($askvars)"
      val unds = if (undvars.isEmpty) "" else s"Und($undvars)"

      val guard = Set(gets,asks,unds,isqs).filterNot(_.isEmpty).mkString(",")

      val upds = gc.cmd.map(Show(_)).mkString(",")

      guard +"~"+ (ins++outs).mkString(",")+"~"+ upds
    case PushTrans(from,push,to) => "" + "~" + s"push(${push})" + "~" + ""
    case PullTrans(from,pull,to) => "" + "~" + s"pull(${pull})" + "~" + ""
  }
}
case class UpdTrans(from:SBState, gc:GuardedCommand, ins:Set[String], outs:Set[String], to:SBState) extends SBTrans {}
case class PushTrans(from:SBState,push:String,to:SBState) extends SBTrans {}
case class PullTrans(from:SBState,pull:String,to:SBState) extends SBTrans {}

/**
  * Companion object for [[SBAutomata]]
  * Helps to build an [[SBAutomata]] from a [[StreamBuilder]]
  */
object SBAutomata {

  def emptySate:SBState = SBState(Set(),Set(),Set())
  def empty:SBAutomata = SBAutomata (Set(),emptySate,Set(),StreamBuilder.empty)

  def apply(sb:StreamBuilder, timeout:Int=5000, mode:BuildMode=NoneMode):SBAutomata = {
    var steps = 0
    def tick(): Unit =  {
      steps+=1
      if (steps==timeout) throw new
          TimeoutException(s"When generating an automata for stream builder:\n ${Show(sb)}")
    }

    // mk initial state from memory variables in sb.init
    val initState = SBState(sb.init.map(c => c.variable),Set(),Set())
    // keep track of all states generated
    var states:Set[SBState] = Set(initState)
    // keep trach of all transitions generated
    var trans:Set[SBTrans] = Set()

    // keep track of the states generated that haven't been visited yet
    var toVisit:Set[SBState] = states
    // keep track of the states generated that have been visited now
    var visited:Set[SBState] = Set()
    while (toVisit.nonEmpty) {
      val toCheck = toVisit
      for (st<-toCheck) {
        tick()
        // make update transitions for guarded commands that satisfy a given state
        val (utrans,usts) = mkUpds(st,sb)
        // updates set of states and transitions
        trans++=utrans
        states++=usts
        toVisit++=usts


        // make push/pull transitions for st if st has no update transitions states and if relevant mode selected
        if (utrans.isEmpty && mode!=NoneMode ) {
          var pushres,pullres:(Set[SBTrans],Set[SBState]) = (Set(),Set())

          if (mode == PushMode || mode == AllMode) pushres = mkPushs(st,sb)
          if (mode == PullMode || mode == AllMode) pullres = mkPulls(st,sb)

          // updates set of states and transitions
          trans++=pushres._1++pullres._1
          states++=pushres._2++pullres._2
          toVisit++=pushres._2++pullres._2
        }
        visited += st
      }
      // update to visit
      toVisit = toVisit -- visited
    }
    SBAutomata(states,initState,trans,sb)
  }


  /**
    * Make update transitions from a given state based on the guarded commands of a stream builder
    * @param from origin state
    * @param sb stream builder
    * @return set of transitions and new states
    */
  private def mkUpds(from:SBState,sb:StreamBuilder):(Set[SBTrans],Set[SBState]) = {
    var trans:Set[SBTrans] = Set()
    var sts:Set[SBState] = Set()
    // make update transitions for guarded commands that satisfy a given state
    for (gc <- sb.gcs ; if satisfies(gc,from,sb)) {
      // inputs get/ask, and outputs variables set
      val getsVars:Set[String] = gc.guard.guards.collect({case g:Get => g}).map(_.variable)
      val askVars  = gc.guard.guards.collect({case g:Ask => g}).map(_.variable)
      // transition synchronized inputs and ouput
      val ins = getsVars ++ askVars
      val outs = gc.outputs //-- sb.memory
      // to state
      val to = SBState(
        (from.memories--getsVars)++gc.outputs.intersect(sb.memory),
        from.activeIns--ins ++ outs.intersect(sb.memory),
        (from.activeOuts--outs) /*++outs.intersect(sb.memory)*/) // todo: check if it makes sens now
      // new transition
      val t = UpdTrans(from,gc,ins--sb.memory,outs--sb.memory,to)
      trans += t
      // update set of states
      sts += to
    }
    (trans,sts)
  }

  /**
    * Make push transitions for input streams from a given state
    * @param from origin state
    * @param sb stream builder
    * @return set of transitions and new states
    */
  private def mkPushs(from:SBState,sb:StreamBuilder):(Set[SBTrans],Set[SBState]) = {
    var trans:Set[SBTrans] = Set()
    var sts:Set[SBState] = Set()
    // make push transitions for st if st has no update transitions states
    for (v<- sb.inputs; if !from.activeIns.contains(v)) {
      val to = SBState(from.memories,from.activeIns+v,from.activeOuts)
      val t = PushTrans(from,v,to)
      trans += t
      sts += to
    }
    (trans,sts)
  }

  /**
    * Make pull transitions for output streams from a given state
    * @param from origin state
    * @param sb stream builder
    * @return set of transitions and new states
    */
  private def mkPulls(from:SBState,sb:StreamBuilder):(Set[SBTrans],Set[SBState]) = {
    var trans:Set[SBTrans] = Set()
    var sts:Set[SBState] = Set()
    // make push transitions for st if st has no update transitions states
    for (v<- sb.outputs; if !from.activeOuts.contains(v)) {
      val to = SBState(from.memories,from.activeIns,from.activeOuts+v)
      val t = PullTrans(from,v,to)
      trans += t
      sts += to
    }
    (trans,sts)
  }

  /**
    * Guarded Command satisfaction
    *
    * @param gc guarded command
    * @param st a stream builder automata state
    * @param sb stream builder
    * @return where the guarded command can be executed in a given state
    */
  private def satisfies(gc:GuardedCommand,st:SBState,sb:StreamBuilder):Boolean = {
    // get inputs of gc (variables in gets and asks in the guard)
    val insg = gc.guard.guards.collect({case g:Get => g; case g:Ask => g}).map(_.variable)
    // get outputs of gc (variables assigned in commands)
    val outsc = gc.cmd.map(_.variable)
    println(s"insg: ${insg} --- activeIns: ${st.activeIns}")
    println(s"outsc: ${outsc} --- activeOuts: ${st.activeOuts}")
    val res = gc.guard.guards.forall(gi=>satisfies(gi,st,sb))  &&
      (insg.intersect(st.activeIns).nonEmpty || outsc.intersect(st.activeOuts).nonEmpty)
    println(s"${Show(gc)} is satisfied by $st: $res")
    res
  }

  /**
    * Guard item satisfaction
    * @param gi guard item
    * @param st a stream builder automata state
    * @param sb stream builder
    * @return where the guard item is satisfy by the given state
    */
  private def satisfies(gi:GuardItem,st:SBState,sb:StreamBuilder):Boolean = gi match {
    case Get(v) if sb.memory.contains(v) => st.memories.contains(v) 
    case Und(v) => !st.memories.contains(v)
    case Ask(v) if sb.memory.contains(v) => st.memories.contains(v)
    case _ => true
  }

}