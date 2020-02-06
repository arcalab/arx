package dsl.analysis.semantics

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
    def alone(gc:GuardedCommand):Boolean =
      ((gc.vars--this.memory) intersect sync).isEmpty

    // checks if two gc can execute synchronously
    def together(gc1:GuardedCommand,gc2:GuardedCommand):Boolean =
      ((gc1.vars--this.memory) intersect sync) == ((gc2.vars--other.memory) intersect sync)

    // composes two guarded commands
    def compose(gc1:GuardedCommand,gc2:GuardedCommand):GuardedCommand = {
      val hide    = gc1.outputs ++ gc2.outputs
      val nguards = hideMix(gc1.guard & gc2.guard,hide)
      val ncmds   = gc1.cmd ++ gc2.cmd

      GuardedCommand(nguards,ncmds)
    }

    // new set of input, output, and memory variables
    val ninit = this.init++other.init
    val nouts = this.outputs++other.outputs
    val nins  = this.inputs++other.inputs -- nouts
    val nmem  = this.memory++other.memory

    var ngcs = Set[GuardedCommand]()

    for (gc <- this.gcs ; if alone(gc))
      ngcs += gc

    for (gc <- other.gcs ; if alone(gc))
      ngcs += gc

    for (gc1 <- this.gcs; gc2 <- other.gcs; if together(gc1,gc2))
      ngcs += compose(gc1,gc2)

    StreamBuilder(ninit,ngcs,nins,nouts,nmem)
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
  private def hideMix(guard:Guard,hide:Set[String]):Guard = guard match {
    case And(g1,g2) => And(hideMix(g1,hide),hideMix(g2,hide))
    case Get(v) => if (hide.contains(v))  True else guard
    case Ask(v) => if (hide.contains(v))  True else guard
    case _ => guard
  }
}

object StreamBuilder {
  type StreamBuilderEntry = (StreamBuilder,List[String],List[String])
  def empty:StreamBuilder = StreamBuilder(Set(),Set())
}
