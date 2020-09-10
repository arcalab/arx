package dsl.analysis.semantics

import dsl.DSL
import dsl.DSL._
import dsl.analysis.semantics.StreamBuilder.StreamBuilderEntry
import dsl.analysis.syntax._
import dsl.analysis.types.TProgram.TBlock
import dsl.analysis.types._
import dsl.backend.ArxNet

/**
  * Created by guillecledou on 2020-02-06
  */


object Encode{

  //type SemanticEncoding = (StreamBuilder,List[String])
  type SemanticResult = (StreamBuilder,List[String],SBContext)

  private var vars:Int = 0
  private def freshVar():String = {vars+=1; s"v${vars-1}"}
  private def freshVarMem():String = {vars+=1; s"m${vars-1}"}

  def apply(program:TProgram, typeCtx:Context,net:ArxNet):SemanticResult = {
    vars = 0
    // create context with primitive functions
    val ctx = loadPrimitives()
    // encode program block
    val (sbB,sbBOuts,sbBCtx) = encode(program.tBlock,ctx,typeCtx,net)
    // filter only relevant outputs
//    println(s"[Encode] filtering $sbBOuts from ${Show(sbB)}")
    val sbB2 = sbB.filterOutAndClean(sbBOuts.toSet)
//    println(s"[Encode] got ${Show(sbB2)}")
    (sbB2,sbBOuts,sbBCtx)
  }

  /**
    * Generates the stream builder of a block and its corresponding sequence of outputs
    * @param block program block
    * @param sbCtx known context of stream builder functions
    * @param typeCtx known context of types for program elements
    * @return a semantic encoding for block
    */
  private def encode(block:TBlock, sbCtx:SBContext, typeCtx:Context, net:ArxNet):SemanticResult = block match {
    case Nil => (StreamBuilder.empty,List(),sbCtx)
    case b::bs =>
      // encode b
      val (sbB,sbBOuts,sbBCtx) = encode(b,sbCtx,typeCtx,net)
      // encode rest and compose it
      val (sbBs,sbBsOuts,sbBsCtx) = encode(bs,sbBCtx,typeCtx,net)
      // compose stream builders and enqueue outputs
      (sbB*sbBs,sbBOuts++sbBsOuts,sbBsCtx)
  }

  /**
    * Generates the stream builder of a statement and its corresponding sequence of outputs
    * @param st program statement
    * @return a semantic encoding for a statement
    */
  private def encode(st:TStatement, sbCtx:SBContext, typeCtx:Context,net:ArxNet):SemanticResult = st match {
    case se:TStreamExpr => encode(se,sbCtx,typeCtx,net)
    // asg.variables <- asg.expression
    case TAssignment(asg, _, rhs) =>
      // get the stream builder entry of the expression
      val (sbE,sbEOuts,sbECtx) = encode(rhs,sbCtx,typeCtx,net)
      // create a map from sbEOuts to variables
      val remap = sbEOuts.zip(asg.variables).toMap
      // remap outputs in the sbE to variables
      val sbFresh = fresh(sbE,remap)
      // add newVar->oldVar to net-mirrors
      for (kv <- remap) net += (kv._1,kv._2)
      // update net with syncs
      for (kv <- remap) net += (Set(kv._1),Set(kv._2),"id")
      // return fresh stream builder
      (sbFresh,List(),sbECtx)
    // x <~ y
    case TRAssignment(RAssignment(List(x),Port(y)), _, _)  =>
      val m = freshVarMem()
      val sbra = sb withCommands(
        ask(m) -> (x := Var(m)),
        get(y) -> (m := Var(y))
      ) ins y outs x mems m
      // add rid:y->x to net
      net += (Set(y),Set(x),"rid")
      (sbra,List(),sbCtx)
    // lhs <~ rhs (generic case)
    case TRAssignment(rasg,tlhs, trhs) =>
      // get fresh variables for the lhs variables
      val freshLhs:List[String] = rasg.variables.map(_=>freshVar())
      // create a regular assignment from the rhs expression to the fresh variables
      val asg = TAssignment(Assignment(freshLhs,rasg.expr),tlhs,trhs)
      // create a new 1-to-1 rasg from fresh variables to original ones
      val nrasgs:List[TRAssignment] = rasg.variables.zip(freshLhs).zip(tlhs)
        .map({case ((x,y),t) => TRAssignment(RAssignment(List(x),Port(y)),List(t),TPort(y,t))})
      // encode new block
      encode(asg::nrasgs,sbCtx,typeCtx,net)
    // def fd
    case TFunDef(fd, _, tb) =>
      val innerNet = new ArxNet
      // get the stream builder of the block
      val (sbB,sbBOuts,sbBCtx) = encode(tb,sbCtx,typeCtx,innerNet)
      // hide all outs but sbBouts and memory variables
//      println(s"[DEF] filtering $sbBOuts from ${Show(sbB)}")
      val sbB2 = sbB.filterOutAndClean(sbBOuts.toSet)
//      println(s"[DEF] got ${Show(sbB2)}")
      // create an stream builder entry for the function
      val fEntry = (sbB2,fd.params.map(tv=>tv.name),sbBOuts,innerNet)
      // add the entry to the context returned by encoding the block
      val nSbCtx = sbBCtx.add(fd.name,fEntry)
      // return the new context and an empty stream builder and outputs
      (StreamBuilder.empty,List(),nSbCtx)
    //todo: case for SFunDef
    case _ =>throw new RuntimeException(s"Statement $st of type ${st.getClass} not supported.")
  }

  private def encode(gt:TGroundTerm, sbCtx: SBContext, net:ArxNet):SemanticResult = gt match {
    case TPort(x,_) =>
      var out = freshVar()
      val gc = Get(x) -> (out := Var(x))
      // update net
      net += (x,out)
      net += (Set(x),Set(out),"id")
      (StreamBuilder(Set(),Set(gc),Set(x),Set(out)),List(out),sbCtx)
    case q@TConst(const,_, _) =>
      val fvq = fv(q)
      val gets = Guard(fvq.map(Get).toSet) //fvq.map(Get).foldRight[Guard](True)(_&_)
      var out = freshVar()
      val gc = gets -> (out := toTerm(const))
      (StreamBuilder(Set(),Set(gc),fvq,Set(out)),List(out),sbCtx)
  }

  /**
    * Generates the stream builder of a stream expression
    * @param se input stream expression
    * @return stream builder for se
    */
  private def encode(se:TStreamExpr, sbCtx:SBContext, typeCtx:Context,net:ArxNet):SemanticResult = se match {
    case gt:TGroundTerm => encode(gt,sbCtx,net)
    case TFunApp(TMatch(TBase(name, _), _),_,targs) =>
      val constructors:List[ConstEntry] = typeCtx.adts(name).constructors
      mkMatch(constructors,targs.head,sbCtx,net)
    case TFunApp(TBuild(_, TBase(name,_)), _, targs) =>
      val constructors:List[ConstEntry] = typeCtx.adts(name).constructors
      mkBuild(constructors,targs,sbCtx,net)
    case TFunApp(TFunName(name,_,data),_, args) =>
      // get the stream builder entry associated to name
      val (sbf,sbIns,sbOuts,sbNetOrig) =
        if (sbCtx.contains(name)) sbCtx(name) else sbCtx("id") //otherwise, assume 1->1 function
      val sbNet = sbNetOrig.clone() // cloning is CRUCIAL! (same MutNet for each primitivie type)
      // if it is unknown name, then use "name" instead of "id" in the net.
      if (!sbCtx.contains(name))
        sbNet.replacePrim("id",name)
      // substitute data params by args if any //todo: for now only supported for primitive functions
      val sb = instantiate(sbf,data)
      // get a stream builder for each argument that is a term
      //val argsSb:List[SemanticResult]  = args.map(a=> encode(a,sbCtx,typeCtx))
      val argsSb:List[Either[SemanticResult,TPort]]  = args.map {
        case a: TPort => Right(a)
        case a => Left(encode(a, sbCtx, typeCtx,net))
      }
      // fresh variables for all variables in sb
      val remap  =  sb.memory              .map(v=> (v,freshVarMem())).toMap ++
                   (sb.inputs ++sb.outputs).map(v=> (v,freshVar())).toMap
      // get a fresh instantiation of the stream builder based on the new name mapping
      val sbFresh = fresh(sb,remap)
      // fix variables used in highlights
      //println(s"\n/-----------------------\\ $name")
      //println(s"[ENC] beginning: $sbNet -- ${Show(sb)}")
      val sbFreshHL = fixHighlights(sbFresh,sbNet)
      //println("Fresh map:\n" + remap.mkString(","))
      // zip fresh inputs with the output of the corresponding argument (we know they have only 1 output)
      val argsNames = argsSb.map(r=> if (r.isLeft) r.left.get._2.head else r.right.get.p)
      val remapInputs:List[(String,String)] = remap.filter(k => sbIns.contains(k._1)).values.toList.zip(argsNames)
      // rename inputs in fresh stream builder based on remapInputs
      val sbRmIns = fresh(sbFreshHL,remapInputs.toMap)
      // compose stream builders from arguments
      val argsComp = argsSb.filter(_.isLeft).map(_.left.get._1).foldRight[StreamBuilder](DSL.sb)(_*_)
      // updating sbNet with remap, and add it to net
      //println("|-----------------------|")
      //println(s"[ENC] replacing for $name: $remap + $remapInputs")
      //println("|-----------------------|")
      //println(s"[ENC] net: fresh+highlights: $sbNet")
      val updNet = sbNet
        .replace(remap)
        .replace(remapInputs.toMap)
//      for (kv <- remapInputs) sbNet += ("RM_"+kv._1,kv._2)
      //println("|-----------------------|")
      //println(s"[ENC] remapNet+: $updNet -- ${Show(sbRmIns * argsComp)}")
      //println("|-----------------------|")
      //println(s"[ENC] final SB: ${Show(sbRmIns * argsComp)}")
      //println("\\-----------------------/\n")
      net ++= updNet
      // return result, and remap outputs of the function to the corresponding fresh names
      (sbRmIns * argsComp,sbOuts.map(remap),sbCtx)
    case _ => throw new RuntimeException(s"Stream Expression $se of type ${se.getClass} not supported.")
    // todo: Any kind of StreamFun if we go back to having this option,
    //    but if will required sequence of inputs as well.
  }

  private def instantiate(sb: StreamBuilder, data: List[TGroundTerm]):StreamBuilder = {
    val map = data.zipWithIndex.map(d=> s"p${d._2+1}" -> toTerm(d._1)).toMap
    val ninit = sb.init.map(c=>instantiate(c,map))
    StreamBuilder(ninit,sb.gcs,sb.inputs,sb.outputs,sb.memory)
  }

  private def instantiate(cmd:Command,map:Map[String,Term]):Command = cmd match {
    case Command(v,Var(n)) => Command(v,map.getOrElse(n,Var(n)))
    case _ => cmd
  }

  private def mkBuild(qs:List[ConstEntry]
                      , args:List[TGroundTerm]
                      , sbCtx:SBContext
                      , net:ArxNet): SemanticResult = {
    val out = freshVar()
    val (gcs,sbs) = mkGCBuild(qs,args,sbCtx,out,net)
    val buildSb = sb withCommands (gcs:_*) outs out ins (gcs.flatMap(gc=>gc.inputs):_*)
    (buildSb*sbs,List(out),sbCtx)
  }

  private def mkGCBuild(qs:List[ConstEntry]
                     , args:List[TGroundTerm]
                     , sbCtx:SBContext,out:String
                     , net:ArxNet): (List[GuardedCommand],StreamBuilder) = qs match {
    case Nil => (List(),DSL.sb)
    case q::more =>
      // get the arguments that correspond to the first constructor q
      val numArgs = q.params.size
      val qArgs = args.take(if (numArgs==0) 1 else numArgs)
      // make a stream builder for each argument that is a term
      //val sbArgs:List[SemanticResult] = qArgs.map(a=> encode(a,sbCtx))
      val sbArgs:List[Either[SemanticResult,TPort]]  = qArgs.map {
        case a: TPort => Right(a)
        case a => Left(encode(a, sbCtx,net))
      }
      // make gets for the output variable of each sb in the previous step
      val getArgs = Guard(sbArgs.map(r => if (r.isLeft) r.left.get._2.head else r.right.get.p)
        .map(p => Get(p)).toSet)
      // remove these arguments from the list
      val restArgs = args.drop(if (numArgs==0) 1 else numArgs)
      // build the constructor
      val argsNames = sbArgs.map(r=> if (r.isLeft) r.left.get._2.head else r.right.get.p)
      val constructor = Const(q.name,if (numArgs==0) List() else argsNames.map(p=> Port(p)))
      // composed arguments stream builders into a single stream builder
      val compArgs =  sbArgs.filter(_.isLeft).map(_.left.get._1).foldRight[StreamBuilder](DSL.sb)(_*_)
      // get the rest of the guarded commands
      val gcMore = mkGCBuild(more,restArgs,sbCtx,out,net)
      // make the actual guarded command for this constructor
      val gcq = getArgs -> (out := toTerm(constructor))
      (gcq::gcMore._1,compArgs*gcMore._2)
  }

  private def mkMatch(qs:List[ConstEntry]
                    , arg:TGroundTerm
                    , sbCtx:SBContext
                    , net:ArxNet): SemanticResult = {
    val (sbIn,in,_) = arg match {
      case a:TPort => (StreamBuilder.empty,List(a.p),sbCtx)
      case _ => encode(arg,sbCtx,net)}
    val (gcs,sbs,outs) = mkGCMatch(qs,sbCtx,in.head)
    val buildSb = sb withCommands (gcs:_*) outs (outs:_*) ins in.head
    // Note: not tested yet
    net += (Set(in.head),outs.toSet,"MATCH")
    (buildSb*sbs*sbIn,outs,sbCtx)
  }

  private def mkGCMatch(qs:List[ConstEntry]
                        , sbCtx:SBContext,in:String): (List[GuardedCommand],StreamBuilder,List[String]) = qs match {
    case Nil => (List(),DSL.sb,List())
    case q::more =>
      // get the arguments that correspond to the first constructor q
      //val numArgs = q.params.size
      // make guard with get for the input variable and guard for isQ
      val guard = Get(in) & IsQ(q.name, Var(in))
      // generate the outputs for this constructor
      var commands:List[Command] = List()
      var outputs:List[String] = List()
      for (p<-q.params.zipWithIndex) {
        val out = freshVar()
        outputs  :+= out
        commands :+= (out := GetQ(q.name,p._2,Var(in)))//(out :=Const(s"get${q.name}${p._2.toString}",Port(in)::Nil))
      }
      if (q.params.isEmpty) {
        val out = freshVar()
        outputs  :+= out
        commands :+= (out := GetQ(q.name,0,Var(in)))//(out := Const(s"get${q.name}0",Port(in)::Nil))
      }
      val gcq = guard -> (commands:_*)
      // get the rest of the guarded commands
      val (gcMore,sbMore,outsMore) = mkGCMatch(more,sbCtx,in)
      (gcq::gcMore,sbMore,outputs++outsMore)
  }

//  /**
//    * Generates the stream builder of a stream function
//    * @param sf
//    * @return
//    */
//  private def encode(sf:StreamFun):SemanticEncoding = sf match {
//    case _ => throw new RuntimeException(s"Stream Function ${Show(sf)} of type ${sf.getClass} not supported.")
////    case FunName(f) =>
////    case ParFun(f1, f2) =>
////    case SeqFun(f1, f2) =>
////    case Match =>
////    case Build =>
//  }

  /**
    * Load primitive functions into the context
    * Each entry in the context is from function name to a [[StreamBuilderEntry]]
    * @return a stream builder context
    */
  private def loadPrimitives():SBContext = {
    val primitives = DSL.prelude.importPrimFunctions()
    var ctx = SBContext()
    for (f <- primitives) {
      ctx = ctx.add(f.name, f.sb)
    }
    ctx
  }

  /**
    * Gets the free variables inside a ground term
    * @param gt ground term
    * @return a set of free variables' names
    */
  private def fv(gt:TGroundTerm):Set[String] = gt match {
    case TPort(x,_) => Set(x)
    case TConst(_,_,targs) => targs.flatMap(fv).toSet
  }

  /**
    * Substitutes a stream builder with fresh names based on a given mapping
    * todo: check if the sets can overlap, then do smart rename to remember new known names
    * @param sb stream builder entry
    * @param remap mapping from names to new names
    * @return sbe substituted based on remap
    */
  private def fresh(sb:StreamBuilder,remap:Map[String,String]):StreamBuilder = {
    val ins  = sb.inputs.map(v=>remap.getOrElse(v,v))
    val outs = sb.outputs.map(v=>remap.getOrElse(v,v))
    val mems = sb.memory.map(v=>remap.getOrElse(v,v))
    val gcs  = sb.gcs.map(gc=>rename(gc,remap))
    val init = sb.init.map(c=>rename(c,remap))
    StreamBuilder(init,gcs,ins,outs,mems)
  }

  private def fixHighlights(builder: StreamBuilder, net: ArxNet): StreamBuilder = {
    val vars = builder.inputs++builder.outputs++builder.memory
    val newVars = builder.gcs.flatMap(_.highlights) -- vars
    val remap = newVars.map((_,freshVar())).toMap
    //println(s"[ENC] fixing highlights: $remap")
    net.replace(remap)
    val res = fresh(builder,remap)
    //println(s"[ENC] got sb: ${Show(res)}")
    res
  }


  private def rename(gc:GuardedCommand,remap:Map[String,String]):GuardedCommand =
    GuardedCommand(rename(gc.guard,remap),gc.cmd.map(c=>rename(c,remap)),gc.highlights.map(rename(_,remap)))

  private def rename(g:Guard,remap:Map[String,String]):Guard = 
    Guard(g.guards.map(gi=>rename(gi,remap)))

  private def rename(g:GuardItem,remap:Map[String,String]):GuardItem = g match {
    case Get(v) if remap.contains(v) => Get(remap(v))
    case Ask(v) if remap.contains(v) => Ask(remap(v))
    case Und(v) if remap.contains(v) => Und(remap(v))
    //case And(g1,g2) => And(rename(g1,remap),rename(g2,remap))
    case IsQ(q,t) => IsQ(q,rename(t,remap)) //if remap.contains(v) => IsQ(q,remap(v))
    case _ => g
  }

  private def rename(s:String,remap:Map[String,String]): String =
    remap.getOrElse(s,s)

  private def rename(c:Command,remap:Map[String,String]):Command =
    Command(rename(c.variable,remap),rename(c.term,remap))

  private def rename(t:Term,remap:Map[String,String]):Term = t match {
    case Var(x) => Var(remap.getOrElse(x,x))
    case Q(name,args) => Q(name,args.map(a=>rename(a,remap)))
    case GetQ(name,index,arg) => GetQ(name,index,rename(arg,remap))
  }

  private def toTerm(gt:GroundTerm):Term = gt match {
    case Port(x) => Var(x)
    case Const(name,args) => Q(name,args.map(a=>toTerm(a)))
  }
  private def toTerm(gt:TGroundTerm):Term = gt match {
    case TPort(x,_) => Var(x)
    case TConst(q,_,targs) => Q(q.q,targs.map(toTerm))
  }
}
