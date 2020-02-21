package dsl.analysis.semantics

import dsl.backend.{Show, Simplify}

/**
  * A stream builder consists of an initial configuration (of commands),
  * and a list of guarded commands
 *
  * @param gcs
  */
case class StreamBuilder(init:Set[Command], gcs:Set[GuardedCommand]
                         , inputs:Set[String]=Set(), outputs:Set[String]=Set(), memory:Set[String]=Set()) {

  /**
    * Composition of stream builders.
    * TODO: assumes for now that there are no name collisions, since sb are created using fresh variables
    * @param other stream builder
    * @return composed stream builder
    */
  def *(other:StreamBuilder):StreamBuilder = {
    // set of ports that must synchronize together
    lazy val sync =
      this.outputs.intersect(other.inputs) ++
      this.inputs.intersect(other.outputs) ++
      this.inputs.intersect(other.inputs)

    // checks if a gc can execute independently
    def alone(gc:GuardedCommand,gcins:Set[String]):Boolean = {
      val r = ((gc.outputs -- this.memory -- gcins) intersect sync).isEmpty
      println(s"Alone ${Show(gc)}: $r (outs: ${gc.outputs.mkString(",")} - sync: ${sync.mkString(",")})")
      r
    }
    //      ((gc.vars--this.memory) intersect sync).isEmpty

    // checks if two gc can execute synchronously
    def together(gc1:GuardedCommand,gc2:GuardedCommand):Boolean =
      ((gc1.vars--this.memory) intersect sync) == ((gc2.vars--other.memory) intersect sync) &&
      outputs.intersect(other.outputs) == Set()

    // composes two guarded commands
    def compose(gc1:GuardedCommand,gc2:GuardedCommand):GuardedCommand = {
      val hide    = gc1.outputs ++ gc2.outputs
      val nguards = hideMix(gc1.guard & gc2.guard,hide) //Simplify(hideMix(gc1.guard & gc2.guard,hide))
      //
//      val nguards = gc1.guard & gc2.guard
      val ncmds   = gc1.cmd ++ gc2.cmd

      GuardedCommand(nguards,ncmds)
    }

    // new set of input, output, and memory variables
    val ninit = this.init++other.init
    val nouts = this.outputs++other.outputs
    val nins  = this.inputs++other.inputs // -- nouts
    val nmem  = this.memory++other.memory

    var ngcs = Set[GuardedCommand]()

    for (gc <- this.gcs ; if alone(gc,this.inputs)) {
      //println(s"Alone ${Show(gc)} (other.I: ${other.inputs.mkString(",")} - other.O: ${other.outputs.mkString(",")})")
      ngcs += gc
    }

    for (gc <- other.gcs ; if alone(gc,other.inputs)) {
      //println(s"Alone ${Show(gc)}")
      ngcs += gc
    }

    for (gc1 <- this.gcs; gc2 <- other.gcs; if together(gc1,gc2)) {
      //println(s"${Show(gc1)} ++ ${Show(gc2)}\n= ${Show(compose(gc1,gc2))} (ok to compose: O1=${outputs} O2=${other.outputs})")
      ngcs += compose(gc1,gc2)
    }

    println(s"Composing:\n  [${gcs.map(Show.apply).mkString(" / ")}]" +
      s"\n  [${other.gcs.map(Show.apply).mkString(" / ")}]" +
      s"\n------------" +
      s"\n  [${ngcs.map(Show.apply).mkString(" / ")}]" +
      s"\n  I:${nins.mkString(",")}  O:${nouts.mkString(",")}")
    StreamBuilder(ninit,ngcs,nins,nouts,nmem)
  }
  /** Leaves only commands that assign `outs` or memory variables. */
  def filterOut(outs:Set[String]): StreamBuilder =
    StreamBuilder(init,gcs.map(filterOut(_,outs ++ memory)),inputs,outputs intersect outs, memory)

  private def filterOut(gc: GuardedCommand, outs:Set[String]): GuardedCommand = {
    val (okCmds,oldCmds) = gc.cmd.partition(outs contains _.variable)
    val oldMap = oldCmds.map(x => x.variable -> x.term).toMap
    val closedCmds = okCmds.map(x => Command(x.variable,closeTerm(x.term,oldMap)))
    GuardedCommand(gc.guard,closedCmds)
  }
  private def closeTerm(term: Term, cmds: Map[String, Term]): Term = term match {
    case Var(name) if cmds contains name => closeTerm(cmds(name),cmds)
    case _:Var  => term
    case Q(name, args) =>Q(name,args.map(closeTerm(_,cmds)))
    case GetQ(name, index, term2) => GetQ(name,index,closeTerm(term2,cmds))
  }


  def ins(is:String*):StreamBuilder =
    StreamBuilder(this.init,this.gcs,this.inputs++is.toSet,this.outputs,this.memory)

  def outs(os:String*):StreamBuilder =
    StreamBuilder(this.init,this.gcs,this.inputs,this.outputs++os.toSet,this.memory)

  def mems(ms:String*):StreamBuilder =
    StreamBuilder(this.init,this.gcs,this.inputs,this.outputs,this.memory++ms.toSet)

  def initially(cmds:Command*):StreamBuilder =
    StreamBuilder(this.init++cmds.toSet,this.gcs,this.inputs,this.outputs,this.memory)

  def withCommands(gcs:GuardedCommand*):StreamBuilder =
    StreamBuilder(this.init,this.gcs++gcs.toSet,this.inputs,this.outputs,this.memory)


  /**
    * After composition of gc, hides all variables that were input streams but are now both,
    * input and output streams.
    * @param guard
    * @param hide
    * @return new guard without mix ports in [Get] and [Und] guards
    */
  private def hideMix(guard:Guard,hide:Set[String]):Guard = 
    Guard(guard.guards.flatMap(gi=>hideMix(gi,hide)))

  /**
    * After composition of gc, hides all variables that were input streams but are now both,
    * input and output streams.
    * @param guard guard item 
    * @param hide variable names to hide
    * @return new guard without mix ports in [Get] and [Und] guards
    */
  private def hideMix(guard:GuardItem,hide:Set[String]):Set[GuardItem] = guard match {
    case Get(v) if (hide.contains(v))  => Set() //else guard
    case Ask(v) if (hide.contains(v))  => Set() //else guard
    case _ => Set(guard)
  }
}

object StreamBuilder {
  type StreamBuilderEntry = (StreamBuilder,List[String],List[String])
  def empty:StreamBuilder = StreamBuilder(Set(),Set())
}
