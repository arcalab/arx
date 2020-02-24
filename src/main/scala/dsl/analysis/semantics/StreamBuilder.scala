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
//    println(s"====\nI1:${inputs.mkString(",")}  O1:${outputs.mkString(",")}\nI2:${other.inputs.mkString(",")}  O2:${other.outputs.mkString(",")}")
    // set of ports that must synchronize together
    lazy val sync =
      this.outputs.intersect(other.inputs) ++
      this.inputs.intersect(other.outputs) ++
      this.inputs.intersect(other.inputs) --
      this.memory -- other.memory

    // checks if a gc can execute independently
    def alone(gc:GuardedCommand, sbins:Set[String], osbins:Set[String]):Boolean = {
      val r = (gc.vars intersect osbins).isEmpty
//      println(s"Alone ${Show(gc)}: $r\n (outs:${gc.outputs.mkString(",")} - sbIns:${sbins.mkString(",")} - sync:${sync.mkString(",")})")
      r
    }

    // checks if two gc can execute synchronously
    def together(gc1:GuardedCommand,gc2:GuardedCommand):Boolean = {
      val r =
        gc1.outputs.intersect(sync) .subsetOf(gc2.inputs) &&
        gc2.outputs.intersect(sync) .subsetOf(gc1.inputs) &&
        gc1.inputs .intersect(sync) .subsetOf(gc2.vars) &&
        gc1.inputs .intersect(sync) .subsetOf(gc1.vars) &&
        gc1.outputs.intersect(gc2.outputs).isEmpty
      //val r = r1 && r2 && r3 && r4 && r5
//      println(s"Together ${Show(gc1)} * ${Show(gc2)}: $r\n" + //($r1,$r2,$r3,$r4,$r5)" +
//        s"(vars1: ${gc1.vars.mkString(",")} - vars2: ${gc2.vars.mkString(",")} - sync: ${sync.mkString(",")})" +
//        s"\n  (outs1: ${outputs}, outs2: ${other.outputs}")
      r
    }

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

    for (gc <- this.gcs ; if alone(gc,this.inputs,other.inputs))
      ngcs += gc

    for (gc <- other.gcs ; if alone(gc,other.inputs,this.inputs))
      ngcs += gc

    for (gc1 <- this.gcs; gc2 <- other.gcs; if together(gc1,gc2))
      ngcs += compose(gc1,gc2)

//    println(s"Composing:\n  [${gcs.map(Show.apply).mkString(" / ")}]" +
//      s"\n  [${other.gcs.map(Show.apply).mkString(" / ")}]" +
//      s"\n------------" +
//      s"\n  [${ngcs.map(Show.apply).mkString(" / ")}]" +
//      s"\n  I:${nins.mkString(",")}  O:${nouts.mkString(",")}")
    StreamBuilder(ninit,ngcs,nins,nouts,nmem)
  }
  /** Leaves only commands that assign `outs` or memory variables. */
  def filterOutAndClean(outs:Set[String]): StreamBuilder = {
    filterOut(cleanMix,outs)
  }

  /** optimize commands, by including only `outs` and memory variables,
    * and their minimum dependencies  */
  private def filterOut(sb:StreamBuilder, outs:Set[String]): StreamBuilder = {
    StreamBuilder(sb.init,sb.gcs.map(filterOut(_,outs ++ sb.memory)),sb.inputs,sb.outputs intersect outs, sb.memory)
//    this
  }

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

  /** Discards guarded commands with guards that have mixed streams, i.e.,
    *  streams that are both input and output. */
  def cleanMix: StreamBuilder = {
    val mix = inputs.intersect(outputs) -- memory
    //println(s"clean mix:${mix.mkString(",")}\n  in:${inputs}  out:${outputs}")
    StreamBuilder(init,gcs.filter(g => g.inputs.intersect(mix).isEmpty),inputs--mix,outputs,memory)
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
