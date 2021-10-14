package dsl.analysis.semantics

import dsl.backend.{ArxNet, Show}
import dsl.revised.core.ConvertAutSB._

import javax.print.attribute.standard.MediaSize.Other


/**
  * A stream builder consists of an initial configuration (of commands),
  * and a list of guarded commands
 *
  * @param gcs guarded commands
  */
case class StreamBuilder(init:Set[Command], gcs:Set[GuardedCommand]
                         , inputs:Set[String]=Set(), outputs:Set[String]=Set(), memory:Set[String]=Set()) {


  def *(other: StreamBuilder): StreamBuilder =
    aut2StreamBuilder(sb2Automaton(this) * sb2Automaton(other))

  def filterOutAndClean(outs:Set[String],net: ArxNet = new ArxNet): StreamBuilder =
    aut2StreamBuilder(sb2Automaton(this).hiding)


  /**
    * Composition of stream builders.
    * TODO: assumes for now that there are no name collisions, since sb are created using fresh variables
    * @param other stream builder
    * @return composed stream builder
    */
  def ****(other:StreamBuilder):StreamBuilder = {
    //println(s"----\n${Show(this)}\n  x\n${Show(other)}")

    var ngcs = Set[GuardedCommand]() // new Guarded Commands

    //    println(s"====\nI1:${inputs.mkString(",")}  O1:${outputs.mkString(",")}\nI2:${other.inputs.mkString(",")}  O2:${other.outputs.mkString(",")}")
    // set of ports that must synchronize together
//    lazy val sync =
//      this.outputs.intersect(other.inputs) ++
//      this.inputs.intersect(other.outputs) ++
//      this.inputs.intersect(other.inputs) --
//      this.memory -- other.memory

    // checks if a gc can execute independently
    def alone(gc:GuardedCommand, sbins:Set[String], osbins:Set[String]):Boolean = {
      val r = (gc.vars intersect osbins).isEmpty
//      println(s"Alone ${Show(gc)}: $r\n (outs:${gc.outputs.mkString(",")} - sbIns:${sbins.mkString(",")} - sync:${sync.mkString(",")})")
      r
    }

    // checks if two gc can execute synchronously
    def together(gc1:GuardedCommand,gc2:GuardedCommand):Boolean = {
      val r =
          gc1.outputs.intersect(other.inputs/* --other.outputs*/) .subsetOf(gc2.inputs) &&  // gc1! & A2?  <  gc2? (+gc1?)
          gc2.outputs.intersect(this.inputs/*  --this.outputs*/)  .subsetOf(gc1.inputs) &&  // - (if fails, missing inputs in A2)
          gc1.inputs .intersect(other.inputs/* ++ other.outputs*/) .subsetOf(gc2.vars) &&   // gc1? & A2?  <  gc2VARS
          gc2.inputs .intersect(this.inputs/*  ++ this.outputs*/)  .subsetOf(gc1.vars) &&   // - (if fails, missing vars in A2)
          gc1.outputs.intersect(gc2.outputs).isEmpty                                        // gc1! # gc2! (if fails, incompatible)
        //      val r = r1 && r2 && r3 && r4 && r5
        //      println(s"Together ${Show(gc1)} * ${Show(gc2)}: $r  ($r1,$r2,$r3,$r4,$r5)\n" +
        //        s"(vars1: ${gc1.vars.mkString(",")} - vars2: ${gc2.vars.mkString(",")} - sync: ${sync.mkString(",")})" +
        //        s"\n  (outs1: ${outputs}, outs2: ${other.outputs}")
        //        println(s"Together ${Show(gc1)}(${alone(gc1,this.inputs,other.inputs)}) * ${Show(gc2)}(${alone(gc2,other.inputs,this.inputs)}): $r")
        if (!gc1.inputs .intersect(other.inputs/* ++ other.outputs*/) .subsetOf(gc2.vars))
          println(s"Together ${Show(gc1)} x ${Show(gc2)} needs vars from right side (${gc2.vars})")
        if (!gc2.inputs .intersect(this.inputs/* ++ other.outputs*/) .subsetOf(gc1.vars))
          println(s"Together ${Show(gc1)} x ${Show(gc2)} needs vars from left side (${gc1.vars})")
        if (!gc1.outputs.intersect(other.inputs/* --other.outputs*/) .subsetOf(gc2.inputs))
          println(s"Together ${Show(gc1)} x ${Show(gc2)} needs inputs from right side (${gc2.inputs})")
        if (!gc2.outputs.intersect(this.inputs/* --other.outputs*/) .subsetOf(gc1.inputs))
          println(s"Together ${Show(gc1)} x ${Show(gc2)} needs inputs from left side (${gc1.inputs})")
      r
    }


    // composes two guarded commands
    def compose(gc1:GuardedCommand,gc2:GuardedCommand):GuardedCommand = {
      val hide    = gc1.outputs ++ gc2.outputs
      val nguards = hideMix(gc1.guard & gc2.guard,hide) //Simplify(hideMix(gc1.guard & gc2.guard,hide))
      //
//      val nguards = gc1.guard & gc2.guard
      val ncmds   = gc1.cmd ++ gc2.cmd

      val nhl = gc1.highlights ++ gc2.highlights

      GuardedCommand(nguards,ncmds, nhl)
    }

    // new set of input, output, and memory variables
    val ninit = this.init++other.init
    val nouts = this.outputs++other.outputs
    val nins  = this.inputs++other.inputs // -- nouts
    val nmem  = this.memory++other.memory

    for (gc <- this.gcs ; if alone(gc,this.inputs,other.inputs))
      ngcs += gc

    for (gc <- other.gcs ; if alone(gc,other.inputs,this.inputs))
      ngcs += gc

    // ORIGINAL COMPOSITION
    for (gc1 <- this.gcs; gc2 <- other.gcs; if together(gc1,gc2))
      ngcs += compose(gc1,gc2)

    //////////////////////////////
    // EXPERIMENTAL COMPOSITION //
    //////////////////////////////
//    sealed abstract class CanTogether
//    case object Ok extends CanTogether
//    case object Nope extends CanTogether
//    case object Miss1 extends CanTogether
//    case object Miss2 extends CanTogether
//
//    def bothCanGo(gc1:GuardedCommand,gc2:GuardedCommand): Boolean =
//    //alone(gc1,this.inputs,other.inputs) && alone(gc2,other.inputs,this.inputs)
//      (ngcs contains gc1) && (ngcs contains gc2)
//
//    def mbTogether(gc1:GuardedCommand,gc2:GuardedCommand): CanTogether = {
//      print(s"Together ${Show(gc1)} x ${Show(gc2)} - ")
//      if ( !noConflict(gc1,gc2) )//gc1.outputs.intersect(gc2.outputs).nonEmpty)  // gc1! # gc2! (if fails, incompatible)
//      { println(s"nope (conflict)")
//        Nope}
//      else if (!gc2.outputs.intersect(this.inputs) .subsetOf(gc1.inputs) || // maybe need to add more...
//        !gc2.inputs .intersect(this.inputs) .subsetOf(gc1.vars))
//      { println(s"needs more from left side (${gc2.outputs.mkString(",")}!/${gc2.inputs.mkString(",")}? ^ ${
//        inputs.mkString(",")} < ${gc1.inputs.mkString(",")}?/${gc1.vars.mkString(",")}) ")
//        Miss1}
//      else if (!gc1.outputs.intersect(other.inputs) .subsetOf(gc2.inputs) ||
//        !gc1.inputs .intersect(other.inputs) .subsetOf(gc2.vars))
//      { println(s"needs more from right side (${gc1.outputs.mkString(",")}!/${gc1.inputs.mkString(",")}? ^ ${
//        other.inputs.mkString(",")} < ${gc2.inputs.mkString(",")}?/${gc2.vars.mkString(",")}) ")
//        Miss2}
//      else {
//        println(s"Ok!\n  - ${Show(compose(gc1,gc2))}")
//        Ok
//      }
//    }
//
//    var miss1 = Set[(GuardedCommand,GuardedCommand)]()
//    var miss2 = Set[(GuardedCommand,GuardedCommand)]()
//    for (gc1 <- this.gcs; gc2 <- other.gcs; if !bothCanGo(gc1,gc2))
//      mbTogether(gc1, gc2) match {
//        case Ok => ngcs += compose(gc1,gc2)
//        case Nope => {}
//        case Miss1 => miss1 += ((gc1,gc2))
//        case Miss2 => miss2 += ((gc1,gc2))
//      }
//    while (miss1.nonEmpty || miss2.nonEmpty) {
//      miss1.headOption match {
//        case Some((nxt1,nxt2)) =>
//          println(s"-- next: maybe adding from [1] to [${Show(nxt1)} || ${Show(nxt2)}]")
//          for (gc1 <- this.gcs
//               if noConflict(gc1,nxt1)) {
//            println(s"1 - Maybe adding ${Show(gc1)}")
//            val gc1b = compose(nxt1,gc1)
//            mbTogether(gc1b,nxt2) match {
//              case Ok => ngcs += compose(gc1b,nxt2)
//              case Nope => {}
//              case Miss1 => miss1 += ((gc1b,nxt2))
//              case Miss2 => miss2 += ((gc1b,nxt1))
//            }
//          }
//          miss1 -= ((nxt1,nxt2))
//        case _ => {}
//      }
//      miss2.headOption match {
//        case Some((nxt1,nxt2)) =>
//          println(s"-- [2] next: maybe adding from [2] to [${Show(nxt1)} || ${Show(nxt2)}]")
//          for (gc2 <- other.gcs
//               if noConflict(gc2,nxt2) ) {
//            println(s"2 - Maybe adding ${Show(gc2)}")
//            val gc2b = compose(gc2,nxt2)
//            mbTogether(nxt1,gc2b) match {
//              case Ok => ngcs += compose(nxt1,gc2b)
//              case Nope => {}
//              case Miss1 => miss1 += ((nxt1,gc2b))
//              case Miss2 => miss2 += ((nxt1,gc2b))
//            }
//          }
//          miss2 -= ((nxt1,nxt2))
//        case _ => {}
//      }
//    }


//    println(s"Composing:\n  [${gcs.map(Show.apply).mkString(" / ")}]" +
//      s"\n  [${other.gcs.map(Show.apply).mkString(" / ")}]" +
//      s"\n------------" +
//      s"\n  [${ngcs.map(Show.apply).mkString(" / ")}]" +
//      s"\n  I:${nins.mkString(",")}  O:${nouts.mkString(",")}")
    StreamBuilder(ninit,ngcs,nins,nouts,nmem)
  }
  /** Leaves only commands that assign `outs` or memory variables. */
  def filterOutAndCleanOLD(outs:Set[String],net: ArxNet = new ArxNet): StreamBuilder = {
    val mix = inputs.intersect(outputs) -- memory
//    println(s"mix to delete: ${mix.mkString(",")}")
    this
      .filterOut(outs,net)
      .cleanMix(mix)
  }

  /** optimize commands, by including only `outs` and memory variables,
    * and their minimum dependencies  */
  private def filterOut(outs:Set[String], net:ArxNet): StreamBuilder = {
    val newGcs = this.gcs.map(filterOut(_, outs ++ this.memory,net))
    StreamBuilder(this.init, newGcs, this.inputs, this.outputs intersect outs, this.memory)
  }

  private def filterOut(gc: GuardedCommand, outs:Set[String],net:ArxNet): GuardedCommand = {
    val (okCmds,oldCmds) = gc.cmd.partition(outs contains _.variable)
    val oldMap = oldCmds.map(x => x.variable -> x.term).toMap
    val closedGuards = gc.guard.guards.flatMap(g=>closeGuard(g,oldMap))
    val closedCmds = okCmds.map(x => Command(x.variable,closeTerm(x.term,oldMap)))
    new GuardedCommand(Guard(closedGuards),closedCmds, gc.highlights)
  }
  private def closeGuard(item: GuardItem,cmds: Map[String,Term]): Set[GuardItem] = item match {
    case IsQ(q, term) => Set(IsQ(q,closeTerm(term,cmds)))
    case Und(q) if cmds contains q => // also propagating Und(q) if (q) is an old variable to be removed.
      println(s"PROPAGATING und($q) [${cmds.mkString(",")}")
      val newUnd = for (x <- cmds(q).vars) yield closeGuard(Und(x),cmds-q)
      newUnd.flatten
    case Und(q) =>
      println(s"NOT PROPAGATING und($q) [${cmds.mkString(",")}")
      Set(item)
    case _ => Set(item)
  }
  private def closeTerm(term: Term, cmds: Map[String, Term]): Term = term match {
    case Var(name) if cmds contains name => closeTerm(cmds(name),cmds-name) // avoiding loops - not sure if it will stop too soon.
    case _:Var  => term
    case Q(name, args) =>Q(name,args.map(closeTerm(_,cmds)))
    case GetQ(name, index, term2) => GetQ(name,index,closeTerm(term2,cmds))
  }

  /** Discards guarded commands with guards that have mixed streams, i.e.,
    *  streams that are both input and output. */
  private def cleanMix(mix:Set[String]): StreamBuilder = {
    //val mix = inputs.intersect(outputs) -- memory
//    println(s"clean mix:${mix.mkString(",")}\n  in:${inputs}  out:${outputs}")
//    println(s"gcs: ${gcs.map{x => Show(x.guard)}.mkString(" / ")} inputs: ${gcs.map(_.inputs)}")
    StreamBuilder(init,gcs.filter(g => g.inputs.intersect(mix).isEmpty),inputs--mix,outputs,memory)
  }

  /** Get undefined variables in a guarded command */
  private def getUnd(gc:GuardedCommand): Set[String] = {
    for (g <- gc.guard.guards if g.isInstanceOf[Und]) yield g.asInstanceOf[Und].v
  }
  /** Check if 2 guarded commands have no conficts (same outputs or inconsistent get/und) */
  private def noConflict(gc1:GuardedCommand,gc2:GuardedCommand): Boolean = {
    gc1.outputs.intersect(gc2.outputs).isEmpty &&
      (gc1.inputs++gc2.inputs).intersect(getUnd(gc1)++getUnd(gc2)).isEmpty
  }

  def ins(is:Iterable[String]):StreamBuilder =
    StreamBuilder(this.init,this.gcs,this.inputs++is.toSet,this.outputs,this.memory)
  def ins(is:String*):StreamBuilder =
    StreamBuilder(this.init,this.gcs,this.inputs++is.toSet,this.outputs,this.memory)

  def outs(os:Iterable[String]):StreamBuilder =
    StreamBuilder(this.init,this.gcs,this.inputs,this.outputs++os.toSet,this.memory)
  def outs(os:String*):StreamBuilder =
    StreamBuilder(this.init,this.gcs,this.inputs,this.outputs++os.toSet,this.memory)

  def mems(ms:String*):StreamBuilder =
    StreamBuilder(this.init,this.gcs,this.inputs,this.outputs,this.memory++ms.toSet)

  def initially(cmds:Command*):StreamBuilder =
    StreamBuilder(this.init++cmds.toSet,this.gcs,this.inputs,this.outputs,this.memory)

  def withCommands(gcs:Iterable[GuardedCommand]):StreamBuilder =
    StreamBuilder(this.init,this.gcs++gcs.toSet,this.inputs,this.outputs,this.memory)
  def withCommands(gcs:GuardedCommand*):StreamBuilder =
    StreamBuilder(this.init,this.gcs++gcs.toSet,this.inputs,this.outputs,this.memory)


  /**
    * After composition of gc, hides all variables that were input streams but are now both,
    * input and output streams.
    * @param guard input guard to be transformed
    * @param hide mixed ports that should be hidden
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
    case Get(v) if hide.contains(v)  => Set() //else guard
    case Ask(v) if hide.contains(v)  => Set() //else guard
    case _ => Set(guard)
  }
}

object StreamBuilder {
  type StreamBuilderEntry = (StreamBuilder,List[String],List[String],ArxNet)
  def empty:StreamBuilder = StreamBuilder(Set(),Set())
}
