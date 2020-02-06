package dsl.backend

import dsl.DSL
import dsl.analysis.syntax.{Const, GroundTerm, Port}
import dsl.analysis.syntax.Program.Block
import dsl.backend.BuildContext.NetBuilder
import dsl.backend.Net.{FunBlock, IPort, Interface}
import dsl.backend.PType

import scala.collection.mutable

object BuildContext {
  type IO = (Interface,Interface)
  type NetBuilder = IO => Net


  def mkPrim(prim:PrimFun): (String, (NetBuilder, Int, Int)) =
    prim.name -> ((io:IO) => Net.mkNet(prim.name,io._1,io._2) , prim.sb._1.inputs.size, prim.sb._1.outputs.size)
  def reoPrims: mutable.Map[String, (NetBuilder, IPort, IPort)] =
    mutable.Map(DSL.prelude.importPrimFunctions().map(mkPrim):_*)
}

class BuildContext {


  val ports: mutable.Map[String, (IPort,PType)]        = mutable.Map()
  val fun:   mutable.Map[String, FunBlock]             = mutable.Map()
  val prims: mutable.Map[String, (NetBuilder,Int,Int)] = BuildContext.reoPrims
  protected var seed:IPort = 0
  def fresh:IPort = {
    seed += 1
    seed
  }
  def freshPort(t:PType): Port = {
    seed += 1
    val pn = "x$"+seed
    ports += pn -> (seed,t)
    Port(pn)
  }
  def getPort(n:String,t:PType): IPort = ports.get(n) match {
    case Some((x,In)) =>
      if(t==Out) ports += n -> (x,Mix)
      x
    case Some((x,Out)) =>
      if(t==In) ports += n -> (x,Mix)
      x
    case Some((x,Mix)) => x
    case None =>
      ports += n -> (fresh,t)
      seed
  }
  def getPorts(gt:GroundTerm,t:PType): List[IPort] = gt match {
    case Port(x) => List(getPort(x,t))
    case Const(_, args) => args.flatMap(getPorts(_,t))
  }
  def cleanedPorts: BuildContext = {
    val b = this
    new BuildContext {
      seed = b.seed
      override val fun: mutable.Map[String, (List[String], Block)] =
        b.fun.clone()
      override val prims: mutable.Map[String, (NetBuilder, IPort, IPort)] =
        b.prims.clone()
    }
  }
  /** Updates state based on the context after evaluating an  */
  def updAppl(other:BuildContext, interface: Interface): Unit = {
    seed = other.seed
//    for (p <- other.ports)
//      if (interface contains p._2._1) ports += p
  }
  /** Maximum value for port used. */
  def maxPort: IPort = seed

  override def toString: String = {
    s"{$seed} - ports: "+ports.mkString(",")+
      " / funs: "+fun.map("<"+_._1+">").mkString(",")//+
      //" / prims: "+prims.map(x=>s"${x._1}:${x._2._2}->${x._2._3}").mkString(",")
  }
}
