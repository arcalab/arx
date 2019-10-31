package dsl.backend

import dsl.analysis.syntax.Program.Block
import dsl.analysis.syntax._
import dsl.backend.Net.{Connector, Interface}


/**
  * Network of primitives - to be translated as a preo.Network, that can in turn be
  * simplified, translated into a preo.Circuit, and depicted using a JS library.
  */
case class Net(prims:List[Connector], ins:Interface, outs:Interface) {
  def ++(other:Net): Net =
    Net(prims++other.prims, ins++other.ins, outs++other.outs)

  override def toString: String =
    prims.map(c => "  "+c.name+" ["+c.ins.mkString(",")+"] -> ["+c.out.mkString(",")+"]\n").mkString +
      "IN : "+ins.mkString(",")+"\n"+
      "OUT: "+outs.mkString(",")
}

object Net {

  case class Connector(name: String, ins: Interface, out: Interface)
  type IPort = Int
  type Interface = Set[IPort]
  type FunBlock = (List[String],Block)



//  class BuildContext() {
//
//  }

  sealed abstract class PType
  case object In extends PType
  case object Out extends PType
  case object Mix extends PType

  //private var seed = 0

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
        case Assignment2(vars, expr) =>
          assgnToNet(vars, expr) ++ apply(tail)
        case FunctionApp(sfun, args) =>
          val res = funAppToNet(sfun, args, tail)
          //println(s"got funAppToNet - $res")
          res
        case FunDef2(name, params, _, block) =>
          gm.fun += name -> (params.map(_.name), block)
          apply(tail)
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
//    vars.foreach(p => gm.getPort(p,Out))
//    Net(netE.prims,Nil,Nil)
  }

  def funAppToNet(fun: StreamFun, args: List[GroundTerm], rest: Block)
                 (implicit gm: BuildContext): Net = {
    val gm2 = gm.cleanedPorts
    //println(s"<$fun> ")
    fun match {
      case FunName(nm) =>
        val hide = nm.lastOption.contains('_')
        val name = if (hide) nm.dropRight(1) else nm
        gm.fun.get(name) match {

          // KNOWN FUNCTION with a Block - evaluate it
          case Some((formalArgs, block)) =>
            //print(s"FN-$name($formalArgs,$block ")
//            val block1 = replace(...)
            val varsInArgs =
              for (a<-args if a.isInstanceOf[Port])
              yield a.asInstanceOf[Port].x
            val block1 = replace(varsInArgs.zip(varsInArgs.map(v=>Port(v+"$"))).toMap,block)
            val block2 = replace(formalArgs.zip(args).toMap, block1)
            val net1 = apply(block2)(gm2) // gm2 will have updated seed, new internal funs, new ports
            def isArg(x:IPort): Boolean = args.exists {
              case Port(name) => gm2.ports.get(name).map(_._1) contains x
              case _ => false
            }
            val ins2 = net1.ins.filter(isArg)
            val prims2 = if (hide)
                List(Connector(name,ins2,net1.outs))
              else
                net1.prims

            val net2 = Net(prims2,ins2,net1.outs)
            gm.updAppl(gm2,net2.ins) // import relevant aspects from gm2
            val restNet = apply(rest)(gm)
            val res = net2 ++ restNet
            res

          // PRIMITIVE FUNCTION
          case None => gm.prims.get(name) match {
            case Some((netFun,_,out)) =>
              val outs = List.fill(out)(gm.fresh).toSet
              val (netArgs,newIns) = processArgs(args)
              netArgs ++ netFun(newIns , outs) ++ apply(rest)
            // UNKNOWN - must be an ID channel
            case None =>
//              val (newPorts,nets) = args.map(mkPort).unzip
//              val newIns = newPorts.filter(_._2!=Out).map(_._1)
////              val newOuts = newPorts.filter(_._2!=In).map(_._1)
  //              val netArgs = nets.fold(Net(Nil,Set(),Set()))(_++_)
              val (netArgs,newIns) = processArgs(args)
              netArgs ++ mkNet(name,newIns,Set(gm.fresh)) ++ apply(rest)
          }

            // if type checks, it must be a named-ID channel
            //            Net(List(Connector(name,1,1)),...)
//            val block2 = replaceIP(Map("in"->args.head),)
//            val cs =
//            Net(List(Connector(name,1,1)), args.flatMap(a=>gm.getPorts(a,In)), List(gm.fresh))
        }
      case Build =>
        val (netArgs,newIns) = processArgs(args)
        netArgs ++ mkNet("BUILD",newIns,Set(gm.fresh)) ++ apply(rest)
      case Match => throw new RuntimeException("Match not supported yet.")
    }
  }

  private def processArgs(args: List[GroundTerm])(implicit gm:BuildContext): (Net,Interface) = {
    val (newPorts,nets) = args.map(mkPort).unzip
    var newIns = newPorts.filter(_._2!=Out).map(_._1)
    val outNets = newPorts.filter(_._2==Out).map(_._1).map(p => {
      val i = gm.fresh
      newIns ::= i
      Net(List(Connector("idd",Set(p),Set(i))),Set(),Set())
    })
    //val newOuts = newPorts.filter(_._2!=In).map(_._1)
    ( (nets++outNets).fold(Net(Nil,Set(),Set()))(_++_) ,
      newIns.toSet)
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
//    Net(List(Connector(Show(Const(q,args))
//                      ,args.count(_.isInstanceOf[Port])
//                      ,1))
//       ,for(a<-args if a.isInstanceOf[Port])
//         yield gm.getPort(a.asInstanceOf[Port].x,In)
//       ,Nil)
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
//    gm.ports.get(p) match {
//      case Some((x, In)) =>
//        gm.ports += p -> (x, Mix)
//        Net(Nil, List(x), List(x))
//      case Some((x, Mix)) =>
//        Net(Nil, List(x), List(x))
//      case Some((x, Out)) =>
//        Net(Nil, List(x), List(x))
//      case None =>
//        val x = gm.fresh
//        gm.ports += p -> (x, Out)
//        Net(Nil, List(x), List(x))
//    }


  def mkPort(gt:GroundTerm)(implicit gm:BuildContext): ((IPort,PType),Net) = gt match {
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

  def replace(sigma: Map[String, GroundTerm], block: Block): Block =
    block.map(replace(sigma,_))
  def replace(sigma: Map[String,GroundTerm], gt: GroundTerm): GroundTerm = gt match {
    case Port(x) => sigma.getOrElse(x,Port(x))
    case Const(q, args) =>Const(q,args.map(replace(sigma,_)))
  }
  def replace(sigma: Map[String,GroundTerm], se: StreamExpr): StreamExpr = se match {
    case term: GroundTerm => replace(sigma,term)
    case FunctionApp(sfun, args) =>
      FunctionApp(sfun,args.map(replace(sigma,_)))
  }
  def replace(sigma: Map[String,GroundTerm], st: Statement): Statement = st match {
    case se: StreamExpr => replace(sigma,se)
    case FunDef2(name, params, typ, block) =>
      FunDef2(name,params,typ,replace(sigma -- params.map(_.name),block))
    case Assignment2(variables, expr) =>
      Assignment2(variables.map(v => sigma.get(v) match {
        case Some(Port(x)) => x
        case Some(c@Const(_,_)) =>
          throw new RuntimeException(s"Cannot replace a sink variable $v by a constant ($c)")
        case None => v
      }), replace(sigma,expr))
  }

  def replace(sigma:Map[IPort,IPort])(n:Net): Net =
    Net(n.prims.map(replaceC(sigma)),n.ins.map(replaceIP(sigma)),n.outs.map(replaceIP(sigma)))
  def replaceIP(sigma:Map[IPort,IPort])(p:IPort): IPort =
    sigma.getOrElse(p,p)
  def replaceC(sigma:Map[IPort,IPort])(c:Connector): Connector =
    Connector(c.name,c.ins.map(replaceIP(sigma)),c.out.map(replaceIP(sigma)))
}