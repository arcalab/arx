//package dsl.backend

// DEPRECATED

//import dsl.analysis.syntax._
//import dsl.analysis.types.TProgram.TBlock
//import dsl.analysis.types._
//import dsl.backend.Net.{Connector, Interface}
//import dsl.common.PatternMatchingException
//
//
///**
//  * Network of primitives - to be translated as a preo.Network, that can in turn be
//  * simplified, translated into a preo.Circuit, and depicted using a JS library.
//  */
//case class Net(prims:List[Connector], ins:Interface, outs:Interface) {
//  def ++(other:Net): Net =
//    Net(prims++other.prims, ins++other.ins, outs++other.outs)
//
//  override def toString: String =
//    prims.map(c => s"${c.name}:${c.ins.mkString(",")}->${c.out.mkString(",")}").mkString("; ") +
//      s" [${ins.mkString(",")}->${outs.mkString(",")}]"
//
//  def pretty: String =
//    prims.map(c => "  "+c.name+" ["+c.ins.mkString(",")+"] -> ["+c.out.mkString(",")+"]\n").mkString +
//      "IN : "+ins.mkString(",")+"\n"+
//      "OUT: "+outs.mkString(",")
//}
//
//object Net {
//
//  case class Connector(name: String, ins: Interface, out: Interface)
//  type IPort = Int
//  type Interface = Set[IPort]
//  type FunBlock = (List[String],TBlock)
//
////  sealed abstract class PType
////  case object In extends PType
////  case object Out extends PType
////  case object Mix extends PType
//
//  def apply(prog: TProgram): (Net,Int) = {
////    println(s"--- starting prog-to-net --- ")
//    val gamma = new BuildContext()
//    val net = apply(prog.tBlock)(gamma)
//    (cleanInputs(net)(gamma),gamma.maxPort)
//  }
//
//  private def cleanInputs(net: Net)(implicit gm: BuildContext): Net = {
//    val portTypes = gm.ports.values.toMap
////    println(s"CLEANing inputs: ${gm.ports.mkString(",")} -> ${
////      net.ins.filter(p => portTypes.get(p) contains In).mkString(",")}")
////    println(s"out ports: ${net.outs}")
//    Net(net.prims,net.ins.filter(p => portTypes.get(p) contains In),net.outs)
//  }
//
//  def apply(b: TBlock)(implicit gm: BuildContext): Net = b match {
//    case Nil => Net(Nil, Set(), Set())
//    case st :: tail =>
//      //println(s"[[${st}]] ")
//      st match {
//        case TPort(x,_) =>
//          portToNet(x) ++ apply(tail)
//        case TConst(Const(q, args),_,_) =>
//          constToNet(q, args) ++ apply(tail)
//        case TAssignment(Assignment(vars, _),_,texpr) =>
//          assgnToNet(vars, texpr) ++ apply(tail)
//        case TRAssignment(RAssignment(vars, _),_,texpr) =>
//          assgnToNet(vars, texpr,reactive=true) ++ apply(tail)
//        case TFunApp(tsfun,_,tIn) =>
//          val res = funAppToNet(tsfun, tIn, tail)
//          //println(s"got funAppToNet - $res")
//          res
//        case TFunDef(fd,t,tblock) =>
//          gm.fun += fd.name -> (fd.params.map(_.name), tblock)
//          apply(tail)
//        //TODO: add SFunDef
//        case TSFunDef(name,_,_) =>
//          throw new PatternMatchingException(s"def $name: pointfree functions not supported when depicting diagrams.")
//      }
//  }
//
//  def assgnToNet(vars: List[String], expr: TStreamExpr,reactive:Boolean=false)
//                (implicit gm:BuildContext): Net = {
//
//    val netE: Net = expr match { //apply(List(expr))
//      case TPort(nm,_) =>
//        val x = gm.getPort(nm,In)
//        val out = gm.getPort(vars.head,Out) // vars must be singleton to type check
//        Net(List(Connector(if (reactive) "rid" else "id",Set(x),Set(out))),Set(x),Set())
//      case _ =>
//        var e = apply(List(expr))
//        if (reactive) {
//          val nouts = e.outs.zip(e.outs.map(p=> gm.fresh))
//          val reactConns =  nouts.map(o => Connector("rid",Set(o._1),Set(o._2)))
//          val reactNet = Net(reactConns.toList++e.prims,e.ins,nouts.map(_._2))
//          //println(s"expression result: ${e}")
//          //println(s"reactive expression result: ${reactNet}")
//          e = reactNet
//        }
//        e
//    }
//    val sigma: Map[IPort,IPort] = vars.zip(netE.outs)
//        .map((pair:(String,IPort)) => gm.ports.get(pair._1) match {
//          case Some((ip,io)) =>
//            if (io==In) gm.ports += pair._1 -> (ip,Mix)
//            pair._2 -> ip
//          case None =>
//            gm.ports += pair._1 -> (pair._2,Out)
//            pair._2 -> pair._2
//        })
//        .toMap
//    //println(s"[netExp]: ${netE}")
//    val netAssg = replace(sigma)(netE)
//    //println(s"[netAssg]: ${netAssg}")
//    Net(netAssg.prims,netAssg.ins,Set()) // clean outs
//  }
//
//  def funAppToNet(fun: TStreamFun, args: List[TGroundTerm], rest: TBlock)
//                 (implicit gm: BuildContext): Net = {
////    println(s"<${fun}> ")
////    println(s"  * processed Args ${args.mkString(",")}")
////    println(s"  * (pre) $gm")
//    val (netArgs,newIns) = processArgs(args)
////    println(s"  * net: $netArgs")
////    println(s"  * ins: $newIns")
////    println(s"  * (pos) $gm")
//
//    val gm2 = gm.cleanedPorts
//
//    fun match {
//      case TFunName(nm, _, _) =>
//        val hide = nm.lastOption.contains('_')
//        val name = if (hide) nm.dropRight(1) else nm
//        gm.fun.get(name) match {
//
//          // KNOWN FUNCTION with a Block - evaluate it
//          case Some((formalArgs, block)) =>
//            //print(s"FN-$name($formalArgs,$block ")
//            // add to context each formal argument var "y" using new ports from processArgs
//            for ((a,port) <- formalArgs zip newIns)
//              gm2.ports += (a -> port)
//
//            val net1 = apply(block)(gm2) // gm2 will have updated seed, new internal funs, new ports
//            val ins2 = newIns.map(_._1).toSet
//            val prims2 = if (hide)
//                List(Connector(name,ins2,net1.outs))
//              else
//                net1.prims
//
//            val net2 = Net(prims2,ins2,net1.outs)
//            gm.updAppl(gm2,net2.ins) // import relevant aspects from gm2
//            val restNet = apply(rest)(gm)
//            val res = netArgs ++ net2 ++ restNet
//            res
//
//          // PRIMITIVE FUNCTION
//          case None => gm.prims.get(name) match {
//            case Some((netFun,_,out)) =>
//              val outs = List.fill(out)(gm.fresh).toSet
//              netArgs ++ netFun(newIns.map(_._1).toSet , outs) ++ apply(rest)
//            // UNKNOWN - must be an ID channel
//            case None =>
//              netArgs ++ mkNet(name,newIns.map(_._1).toSet,Set(gm.fresh)) ++ apply(rest)
//          }
//        }
//      case TBuild(tin, tout) =>
//        //val (netArgs,newIns) = processArgs(args)
//        netArgs ++ mkNet("BUILD",newIns.map(_._1).toSet,Set(gm.fresh)) ++ apply(rest)
//      case TMatch(tin, tout) =>
//        // get number of outputs from type of match
//        val newOuts = getTensorOuts(tout)
//        //println(s"Match:${Show(tin)} -> ${Show(tout)}\nOutputs:${newOuts.mkString(",")}")
//        netArgs ++ mkNet("MATCH",newIns.map(_._1).toSet,newOuts.toSet) ++ apply(rest)
//        // todo
//        //throw new RuntimeException("Match not supported yet.")
//      case TParFun(_,_) =>
//        throw new RuntimeException("Parallel functions not supported yet by Net construction.")
//      case TSeqFun(_,_) =>
//        throw new RuntimeException("Sequence of functions not supported yet by Net construction.")
//    }
//  }
//
//  private def getTensorOuts(texp:TExp)(implicit gm:BuildContext):List[IPort] = texp match {
//    case TTensor(t1,t2) => getTensorOuts(t1) ++ getTensorOuts(t2)
//    case _ => List(gm.fresh)
//  }
//
//  /** From actual arguments, eg, (x,True), build:
//    *  - Net([id1: []->True_p] , Nil, Nil ) - for the generated port True_p for "True" (and other constructors if they exist)
//    *  - [x_p, True_p] - the port numbers of each actual argument.
//    */
//  private def processArgs(args: List[TGroundTerm])(implicit gm:BuildContext): (Net,List[(IPort,PortType)]) = {
//    val (newPorts,nets) = args.map(mkPort).unzip
//    ( nets.fold(Net(Nil,Set(),Set()))(_++_) ,
//      newPorts)
//  }
//
//  // constructor (constant stream) to a Net
//  def constToNet(q: String, args: List[GroundTerm])
//                (implicit gm:BuildContext): Net = {
//    // get args, find a port for each
//    //   if a constructor, transform it into a primitive with a single outport
//    //   find variables in the
//    // OR!!! assume only values in args, and build a single output and a connector pointing to it (a writer)
//    // (the dev can still use "build" to create complex data structures.
//    //println(s"Building constructor ${q} [${args.mkString("/")}]")
//    val name = Show(Const(q,args))
//    val ins: Interface = getPorts(args)
//    val outs: Interface = Set(gm.fresh)
//    mkNet(name,ins,outs)
//  }
//
//  def getPorts(args: List[GroundTerm])(implicit gm:BuildContext): Interface = args match {
//    case Nil => Set()
//    case Port(x)::tl => getPorts(tl) + gm.getPort(x,In)
//    case Const(_,args2)::tl => getPorts(args2:::tl)
//  }
//
//
//
//  // port as output
//  def portToNet(p: String)//,t:PType)
//               (implicit gm: BuildContext): Net = {
//    val x = gm.getPort(p,Out)
//    Net(Nil,Set(),Set(x))
//  }
//
//  def mkPort(gt:TGroundTerm)(implicit gm:BuildContext): ((IPort,PortType),Net) = gt match {
//    case TPort(x,_) =>
//      gm.getPort(x,In)
//      (gm.ports(x),Net(Nil,Set(),Set()))
//    case TConst(Const(q, args),_,_) =>
//      val nc = constToNet(q,args)
//      ((nc.outs.head,Out),Net(nc.prims,Set(),Set()))
//  }
//
//  def mkNet(nm:String,in:IPort,out:IPort): Net = mkNet(nm,Set(in),Set(out))
//  def mkNet(nm:String,ins:Interface,outs:Interface): Net =
//    Net(List(Connector(nm,ins,outs)),ins,outs)
//
//
//  ////////////////////////
//
////  @deprecated
////  def replace(sigma: Map[String, GroundTerm], block: Block): Block =
////    block.map(replace(sigma,_))
////  @deprecated
////  def replace(sigma: Map[String,GroundTerm], gt: GroundTerm): GroundTerm = gt match {
////    case Port(x) => sigma.getOrElse(x,Port(x))
////    case Const(q, args) =>Const(q,args.map(replace(sigma,_)))
////  }
////  @deprecated
////  def replace(sigma: Map[String,GroundTerm], se: StreamExpr): StreamExpr = se match {
////    case term: GroundTerm => replace(sigma,term)
////    case FunctionApp(sfun, args) =>
////      FunctionApp(sfun,args.map(replace(sigma,_)))
////  }
////  @deprecated
////  def replace(sigma: Map[String,GroundTerm], st: Statement): Statement = st match {
////    case se: StreamExpr => replace(sigma,se)
////    case FunDef2(name, params, typ, block) =>
////      FunDef2(name,params,typ,replace(sigma -- params.map(_.name),block))
////    case Assignment2(variables, expr) =>
////      Assignment2(variables.map(v => sigma.get(v) match {
////        case Some(Port(x)) => x
////        case Some(c@Const(_,_)) =>
////          throw new RuntimeException(s"Cannot replace a sink variable $v by a constant ($c)")
////        case None => v
////      }), replace(sigma,expr))
////  }
//
//  ///
//
//  def replace(sigma:Map[IPort,IPort])(n:Net): Net =
//    Net(n.prims.map(replaceC(sigma)),n.ins.map(replaceIP(sigma)),n.outs.map(replaceIP(sigma)))
//  private def replaceIP(sigma:Map[IPort,IPort])(p:IPort): IPort =
//    sigma.getOrElse(p,p)
//  private def replaceC(sigma:Map[IPort,IPort])(c:Connector): Connector =
//    Connector(c.name,c.ins.map(replaceIP(sigma)),c.out.map(replaceIP(sigma)))
//}
