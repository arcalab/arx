package dsl.backend

import dsl.analysis.syntax.Program.Block
import dsl.analysis.syntax._
import dsl.backend.Net.{Connector, Interface}
import dsl.backend.PortType


/**
  * Network of primitives - to be translated as a preo.Network, that can in turn be
  * simplified, translated into a preo.Circuit, and depicted using a JS library.
  */
case class Net(prims:List[Connector], ins:Interface, outs:Interface) {
  def ++(other:Net): Net =
    Net(prims++other.prims, ins++other.ins, outs++other.outs)

  override def toString: String =
    prims.map(c => s"${c.name}:${c.ins.mkString(",")}->${c.out.mkString(",")}").mkString("; ") +
      s" [${ins.mkString(",")}->${outs.mkString(",")}]"

  def pretty: String =
    prims.map(c => "  "+c.name+" ["+c.ins.mkString(",")+"] -> ["+c.out.mkString(",")+"]\n").mkString +
      "IN : "+ins.mkString(",")+"\n"+
      "OUT: "+outs.mkString(",")
}

object Net {

  case class Connector(name: String, ins: Interface, out: Interface)
  type IPort = Int
  type Interface = Set[IPort]
  type FunBlock = (List[String],Block)

//  sealed abstract class PType
//  case object In extends PType
//  case object Out extends PType
//  case object Mix extends PType

  def apply(prog: Program): (Net,Int) = {
//    println(s"--- starting prog-to-net --- ")
    val gamma = new BuildContext()
    val net = apply(prog.block)(gamma)
    (cleanInputs(net)(gamma),gamma.maxPort)
  }

  private def cleanInputs(net: Net)(implicit gm: BuildContext): Net = {
    val portTypes = gm.ports.values.toMap
//    println(s"CLEANing inputs: ${gm.ports.mkString(",")} -> ${
//      net.ins.filter(p => portTypes.get(p) contains In).mkString(",")}")
//    println(s"out ports: ${net.outs}")
    Net(net.prims,net.ins.filter(p => portTypes.get(p) contains In),net.outs)
  }

  def apply(b: Block)(implicit gm: BuildContext): Net = b match {
    case Nil => Net(Nil, Set(), Set())
    case st :: tail =>
      //println(s"[[${st}]] ")
      st match {
        case Port(x) =>
          portToNet(x) ++ apply(tail)
        case Const(q, args) =>
          constToNet(q, args) ++ apply(tail)
        case Assignment(vars, expr) =>
          assgnToNet(vars, expr) ++ apply(tail)
        case FunctionApp(sfun, args) =>
          val res = funAppToNet(sfun, args, tail)
          //println(s"got funAppToNet - $res")
          res
        case FunDef(name, params, _, block) =>
          gm.fun += name -> (params.map(_.name), block)
          apply(tail)
        //TODO: add SFunDef
      }
  }

  def assgnToNet(vars: List[String], expr: StreamExpr)
                (implicit gm:BuildContext): Net = {

    val netE: Net = expr match { //apply(List(expr))
      case Port(nm) =>
        val x = gm.getPort(nm,In)
        val out = gm.getPort(vars.head,Out) // vars must be singleton to type check
        Net(List(Connector("id",Set(x),Set(out))),Set(x),Set())
      case _ => apply(List(expr))
    }
    val sigma: Map[IPort,IPort] = vars.zip(netE.outs)
        .map((pair:(String,IPort)) => gm.ports.get(pair._1) match {
          case Some((ip,io)) =>
            if (io==In) gm.ports += pair._1 -> (ip,Mix)
            pair._2 -> ip
          case None =>
            gm.ports += pair._1 -> (pair._2,Out)
            pair._2 -> pair._2
        })
        .toMap
    val netAssg = replace(sigma)(netE)
    Net(netAssg.prims,netAssg.ins,Set()) // clean outs
  }

  def funAppToNet(fun: StreamFun, args: List[GroundTerm], rest: Block)
                 (implicit gm: BuildContext): Net = {
//    println(s"<${fun}> ")
//    println(s"  * processed Args ${args.mkString(",")}")
//    println(s"  * (pre) $gm")
    val (netArgs,newIns) = processArgs(args)
//    println(s"  * net: $netArgs")
//    println(s"  * ins: $newIns")
//    println(s"  * (pos) $gm")

    val gm2 = gm.cleanedPorts

    fun match {
      case FunName(nm) =>
        val hide = nm.lastOption.contains('_')
        val name = if (hide) nm.dropRight(1) else nm
        gm.fun.get(name) match {

          // KNOWN FUNCTION with a Block - evaluate it
          case Some((formalArgs, block)) =>
            //print(s"FN-$name($formalArgs,$block ")
            // add to context each formal argument var "y" using new ports from processArgs
            for ((a,port) <- formalArgs zip newIns)
              gm2.ports += (a -> port)

            val net1 = apply(block)(gm2) // gm2 will have updated seed, new internal funs, new ports
            val ins2 = newIns.map(_._1).toSet
            val prims2 = if (hide)
                List(Connector(name,ins2,net1.outs))
              else
                net1.prims

            val net2 = Net(prims2,ins2,net1.outs)
            gm.updAppl(gm2,net2.ins) // import relevant aspects from gm2
            val restNet = apply(rest)(gm)
            val res = netArgs ++ net2 ++ restNet
            res

          // PRIMITIVE FUNCTION
          case None => gm.prims.get(name) match {
            case Some((netFun,_,out)) =>
              val outs = List.fill(out)(gm.fresh).toSet
              netArgs ++ netFun(newIns.map(_._1).toSet , outs) ++ apply(rest)
            // UNKNOWN - must be an ID channel
            case None =>
              netArgs ++ mkNet(name,newIns.map(_._1).toSet,Set(gm.fresh)) ++ apply(rest)
          }
        }
      case Build =>
        //val (netArgs,newIns) = processArgs(args)
        netArgs ++ mkNet("BUILD",newIns.map(_._1).toSet,Set(gm.fresh)) ++ apply(rest)
      case Match => throw new RuntimeException("Match not supported yet.")
    }
  }

  /** From actual arguments, eg, (x,True), build:
    *  - Net([id1: []->True_p] , Nil, Nil ) - for the generated port True_p for "True" (and other constructors if they exist)
    *  - [x_p, True_p] - the port numbers of each actual argument.
    */
  private def processArgs(args: List[GroundTerm])(implicit gm:BuildContext): (Net,List[(IPort,PortType)]) = {
    val (newPorts,nets) = args.map(mkPort).unzip
    ( nets.fold(Net(Nil,Set(),Set()))(_++_) ,
      newPorts)
  }

  // constructor (constant stream) to a Net
  def constToNet(q: String, args: List[GroundTerm])
                (implicit gm:BuildContext): Net = {
    // get args, find a port for each
    //   if a constructor, transform it into a primitive with a single outport
    //   find variables in the
    // OR!!! assume only values in args, and build a single output and a connector pointing to it (a writer)
    // (the dev can still use "build" to create complex data structures.
    //println(s"Building constructor ${q} [${args.mkString("/")}]")
    val name = Show(Const(q,args))
    val ins: Interface = getPorts(args)
    val outs: Interface = Set(gm.fresh)
    mkNet(name,ins,outs)
  }

  def getPorts(args: List[GroundTerm])(implicit gm:BuildContext): Interface = args match {
    case Nil => Set()
    case Port(x)::tl => getPorts(tl) + gm.getPort(x,In)
    case Const(_,args2)::tl => getPorts(args2:::tl)
  }



  // port as output
  def portToNet(p: String)//,t:PType)
               (implicit gm: BuildContext): Net = {
    val x = gm.getPort(p,Out)
    Net(Nil,Set(),Set(x))
  }

  def mkPort(gt:GroundTerm)(implicit gm:BuildContext): ((IPort,PortType),Net) = gt match {
    case Port(x) =>
      gm.getPort(x,In)
      (gm.ports(x),Net(Nil,Set(),Set()))
    case Const(q, args) =>
      val nc = constToNet(q,args)
      ((nc.outs.head,Out),Net(nc.prims,Set(),Set()))
  }

  def mkNet(nm:String,in:IPort,out:IPort): Net = mkNet(nm,Set(in),Set(out))
  def mkNet(nm:String,ins:Interface,outs:Interface): Net =
    Net(List(Connector(nm,ins,outs)),ins,outs)


  ////////////////////////

//  @deprecated
//  def replace(sigma: Map[String, GroundTerm], block: Block): Block =
//    block.map(replace(sigma,_))
//  @deprecated
//  def replace(sigma: Map[String,GroundTerm], gt: GroundTerm): GroundTerm = gt match {
//    case Port(x) => sigma.getOrElse(x,Port(x))
//    case Const(q, args) =>Const(q,args.map(replace(sigma,_)))
//  }
//  @deprecated
//  def replace(sigma: Map[String,GroundTerm], se: StreamExpr): StreamExpr = se match {
//    case term: GroundTerm => replace(sigma,term)
//    case FunctionApp(sfun, args) =>
//      FunctionApp(sfun,args.map(replace(sigma,_)))
//  }
//  @deprecated
//  def replace(sigma: Map[String,GroundTerm], st: Statement): Statement = st match {
//    case se: StreamExpr => replace(sigma,se)
//    case FunDef2(name, params, typ, block) =>
//      FunDef2(name,params,typ,replace(sigma -- params.map(_.name),block))
//    case Assignment2(variables, expr) =>
//      Assignment2(variables.map(v => sigma.get(v) match {
//        case Some(Port(x)) => x
//        case Some(c@Const(_,_)) =>
//          throw new RuntimeException(s"Cannot replace a sink variable $v by a constant ($c)")
//        case None => v
//      }), replace(sigma,expr))
//  }

  ///

  def replace(sigma:Map[IPort,IPort])(n:Net): Net =
    Net(n.prims.map(replaceC(sigma)),n.ins.map(replaceIP(sigma)),n.outs.map(replaceIP(sigma)))
  private def replaceIP(sigma:Map[IPort,IPort])(p:IPort): IPort =
    sigma.getOrElse(p,p)
  private def replaceC(sigma:Map[IPort,IPort])(c:Connector): Connector =
    Connector(c.name,c.ins.map(replaceIP(sigma)),c.out.map(replaceIP(sigma)))
}