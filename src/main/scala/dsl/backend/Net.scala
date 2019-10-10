package dsl.backend

import dsl.analysis.syntax.Program.Block
import dsl.analysis.syntax._
import dsl.backend.Net.{Connector, Interface}

import scala.collection.mutable


/**
  * Network of primitives - to be translated as a preo.Network, that can in turn be
  * simplified, translated into a preo.Circuit, and depicted using a JS library.
  * @param prims
  * @param ins
  * @param outs
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
  type Interface = List[IPort]
  type FunBlock = (List[String],Block)



//  class BuildContext() {
//
//  }

  sealed abstract class PType
  case object In extends PType
  case object Out extends PType
  case object Mix extends PType

  //private var seed = 0

  def apply(prog: Program): Net = {
    println(s"--- starting prog-to-net --- ")
    val gamma = new BuildContext()
    apply(prog.block)(gamma)
  }

  def apply(b: Block)(implicit gm: BuildContext): Net = b match {
    case Nil => Net(Nil, Nil, Nil)
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
          funAppToNet(sfun, args, tail)
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
        Net(List(Connector("id",List(x),List(out))),List(x),Nil)
      case _ => apply(List(expr))
    }
    val sigma: Map[IPort,IPort] = vars.zip(netE.outs)
        .map((pair:(String,IPort)) => gm.ports.get(pair._1) match {
          case Some((ip,_)) => pair._2 -> ip
          case None =>
            gm.ports += pair._1 -> (pair._2,Out)
            pair._2 -> pair._2
        })
        .toMap
    val netAssg = replace(sigma)(netE)
    Net(netAssg.prims,netAssg.ins,Nil) // clean outs
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
            val block2 = replace(formalArgs.zip(args).toMap, block)
            //print(s"recursive-${block2} ")
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
            //print(s"net-$net1 ")
            gm.updAppl(gm2) // import relevant aspects from gm2
            val restNet = apply(rest)(gm)
            net2 ++ restNet
          // PRIMITIVE FUNCTION
          case None => gm.prims.get(name) match {
            case Some((netFun,_,out)) =>
              val (ins,nets) = args.map(mkPort).unzip
              val outs = List.fill(out)(gm.fresh)
              val netArgs = nets.fold(Net(Nil,Nil,Nil))(_++_)
              netArgs ++ netFun(ins,outs) ++ apply(rest)
            // UNKNOWN - must be an ID channel
            case None =>
              val (ins,nets) = args.map(mkPort).unzip
              val netArgs = nets.fold(Net(Nil,Nil,Nil))(_++_)
              netArgs ++ mkNet(name,ins,List(gm.fresh)) ++ apply(rest)
          }


            // if type checks, it must be a named-ID channel
            //            Net(List(Connector(name,1,1)),...)
//            val block2 = replaceIP(Map("in"->args.head),)
//            val cs =
//            Net(List(Connector(name,1,1)), args.flatMap(a=>gm.getPorts(a,In)), List(gm.fresh))
        }
      case Build =>
        val (ins,nets) = args.map(mkPort).unzip
        val netArgs = nets.fold(Net(Nil,Nil,Nil))(_++_)
        netArgs ++ mkNet("BUILD",ins,List(gm.fresh)) ++ apply(rest)
      case Match => throw new RuntimeException("Match not supported yet.")
    }
  }


  // constructor (constant stream) to a Net
  def constToNet(q: String, args: List[GroundTerm])
                (implicit gm:BuildContext): Net = {
    // get args, find a port for each
    //   if a constructor, transform it into a primitive with a single outport
    //   find variables in the
    // OR!!! assume only values in args, and build a single output and a connector pointing to it (a writer)
    // (the dev can still use "build" to create complex data structures.
    val name = Show(Const(q,args))
    val ins: Interface = getPorts(args)
    val outs: Interface = List(gm.fresh)
    mkNet(name,ins,outs)
//    Net(List(Connector(Show(Const(q,args))
//                      ,args.count(_.isInstanceOf[Port])
//                      ,1))
//       ,for(a<-args if a.isInstanceOf[Port])
//         yield gm.getPort(a.asInstanceOf[Port].x,In)
//       ,Nil)
  }

  def getPorts(args: List[GroundTerm])(implicit gm:BuildContext): Interface = args match {
    case Nil => Nil
    case Port(x)::tl => gm.getPort(x,In) :: getPorts(tl)
    case Const(_,args2)::tl => getPorts(args2:::tl)
  }



  // port as output
  def portToNet(p: String)//,t:PType)
               (implicit gm: BuildContext): Net = {
    val x = gm.getPort(p,Out)
    Net(Nil,List(),List(x))
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


  def mkPort(gt:GroundTerm)(implicit gm:BuildContext): (IPort,Net) = gt match {
    case Port(x) => (gm.getPort(x,In),Net(Nil,Nil,Nil))
    case Const(q, args) =>
      val nc = constToNet(q,args)
      (nc.outs.head,nc)
  }

  def mkNet(nm:String,in:IPort,out:IPort): Net = mkNet(nm,List(in),List(out))
  def mkNet(nm:String,ins:List[IPort],outs:List[IPort]): Net =
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