package dsl.analysis.semantics

import dsl.DSL
import dsl.DSL._
import dsl.analysis.semantics.StreamBuilder.StreamBuilderEntry
import dsl.analysis.syntax.Program.Block
import dsl.analysis.syntax._
import dsl.analysis.types.TProgram.TBlock
import dsl.analysis.types._
import dsl.backend.Show

/**
  * Created by guillecledou on 2020-02-06
  */


object Encode{

  //type SemanticEncoding = (StreamBuilder,List[String])
  type SemanticResult = (StreamBuilder,List[String],SBContext)

  private var vars:Int = 0
  private def freshVar():String = {vars+=1; s"v${vars-1}"}

  def apply(program:TProgram, typeCtx:Context):SemanticResult = {
    vars = 0
    // create context with primitive functions
    val ctx = loadPrimitives()
    // encode program block
    val (sbB,sbBOuts,sbBCtx) = encode(program.tBlock,ctx,typeCtx)
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
  private def encode(block:TBlock, sbCtx:SBContext, typeCtx:Context):SemanticResult = block match {
    case Nil => (StreamBuilder.empty,List(),sbCtx)
    case b::bs =>
      // encode b
      val (sbB,sbBOuts,sbBCtx) = encode(b,sbCtx,typeCtx)
      // encode rest and compose it
      val (sbBs,sbBsOuts,sbBsCtx) = encode(bs,sbBCtx,typeCtx)
      // compose stream builders and enqueue outputs
      (sbB*sbBs,sbBOuts++sbBsOuts,sbBsCtx)
  }

  /**
    * Generates the stream builder of a statement and its corresponding sequence of outputs
    * @param st program statement
    * @return a semantic encoding for a statement
    */
  private def encode(st:TStatement, sbCtx:SBContext, typeCtx:Context):SemanticResult = st match {
    case se:TStreamExpr => encode(se,sbCtx,typeCtx)
    case TAssignment(asg,lhs, rhs) =>
      // get the stream builder entry of the expression
      val (sbE,sbEOuts,sbECtx) = encode(rhs,sbCtx,typeCtx)
      // create a map from sbEOuts to variables
      val remap = sbEOuts.zip(asg.variables).toMap
      // remap outputs in the sbE to variables
      val sbFresh = fresh(sbE,remap)
      // return fresh stream builder
      (sbFresh,List(),sbECtx)
    case TRAssignment(RAssignment(List(x),Port(y)),tlhs, trhs)  =>
      val m = freshVar()
      val sbra = sb withCommands(
        ask(m) -> (x := Var(m)),
        get(y) -> (m := Var(y))
      ) ins y outs x mems m
      (sbra,List(),sbCtx)
    case TRAssignment(rasg,tlhs, trhs) =>
      // get fresh variables for the lhs variables
      val freshLhs:List[String] = rasg.variables.map(_=>freshVar())
      // create a regular assignment from the rhs expression to the fresh variables
      val asg = TAssignment(Assignment(freshLhs,rasg.expr),tlhs,trhs)
      // create a new 1-to-1 rasg from fresh variables to original ones
      val nrasgs:List[TRAssignment] = rasg.variables.zip(freshLhs).zip(tlhs)
        .map({case ((x,y),t) => TRAssignment(RAssignment(List(x),Port(y)),List(t),TPort(y,t))})
      // encode new block
      encode(asg::nrasgs,sbCtx,typeCtx)
    case TFunDef(fd, te, tb) =>
      // get the stream builder of the block
      val (sbB,sbBOuts,sbBCtx) = encode(tb,sbCtx,typeCtx)
      // hide all outs but sbBouts and memory variables
//      println(s"[DEF] filtering $sbBOuts from ${Show(sbB)}")
      val sbB2 = sbB.filterOutAndClean(sbBOuts.toSet)
//      println(s"[DEF] got ${Show(sbB2)}")
      // create an stream builder entry for the function
      val fEntry = (sbB2,fd.params.map(tv=>tv.name),sbBOuts)
      // add the entry to the context returned by encoding the block
      val nSbCtx = sbBCtx.add(fd.name,fEntry)
      // return the new context and an empty stream builder and outputs
      (StreamBuilder.empty,List(),nSbCtx)
    //todo: case for SFunDef
    case _ =>throw new RuntimeException(s"Statement ${st} of type ${st.getClass} not supported.")
  }

  private def encode(gt:TGroundTerm, sbCtx: SBContext):SemanticResult = gt match {
    case TPort(x,_) =>
      var out = freshVar()
      var gc  = Get(x) -> (out := Var(x))
      (StreamBuilder(Set(),Set(gc),Set(x),Set(out)),List(out),sbCtx)
    case q@TConst(const,_,targs) =>
      var fvq = fv(q)
      var gets = Guard(fvq.map(Get).toSet) //fvq.map(Get).foldRight[Guard](True)(_&_)
      var out = freshVar()
      var gc = gets -> (out := toTerm(const))
      (StreamBuilder(Set(),Set(gc),fvq,Set(out)),List(out),sbCtx)
  }

  /**
    * Generates the stream builder of a stream expression
    * @param se
    * @return stream builder for se
    */
  private def encode(se:TStreamExpr, sbCtx:SBContext, typeCtx:Context):SemanticResult = se match {
    case gt:TGroundTerm => encode(gt,sbCtx)
    case TFunApp(TMatch(TBase(name, _), _),_,targs) =>
      val constructors:List[ConstEntry] = typeCtx.adts(name).constructors
      mkMatch(constructors,targs.head,sbCtx)
    case TFunApp(TBuild(_, TBase(name,_)), _, targs) =>
      val constructors:List[ConstEntry] = typeCtx.adts(name).constructors
      mkBuild(constructors,targs,sbCtx)
    case TFunApp(TFunName(name,_),_, args) =>
      // get the stream builder entry associated to name
      val (sb,sbIns,sbOuts) = if (sbCtx.contains(name)) sbCtx(name) else sbCtx("id") //otherwise, assume 1->1 function
      // get a stream builder for each argument that is a term
      //val argsSb:List[SemanticResult]  = args.map(a=> encode(a,sbCtx,typeCtx))
      val argsSb:List[Either[SemanticResult,Var]]  = args.map(a=> { a match {
        case a:Var => Right(a)
        case _ => Left(encode(a,sbCtx,typeCtx))
      }})
      // fresh variables for all variables in sb
      val remap  = (sb.inputs ++sb.outputs++sb.memory).map(v=> (v,freshVar())).toMap
      // get a fresh instantiation of the stream builder based on the new name mapping
      val sbFresh = fresh(sb,remap)
      //println("Fresh map:\n" + remap.mkString(","))
      // zip fresh inputs with the output of the corresponding argument (we know they have only 1 output)
      val argsNames = argsSb.map(r=> if (r.isLeft) r.left.get._2.head else r.right.get.name)
      val remapInputs:List[(String,String)] = remap.filter(k => sbIns.contains(k._1)).values.toList.zip(argsNames)
      // rename inputs in fresh stream builder based on remapInputs
      val sbRmIns = fresh(sbFresh,remapInputs.toMap)
      // compose stream builders from arguments
      val argsComp = argsSb.filter(_.isLeft).map(_.left.get._1).foldRight[StreamBuilder](DSL.sb)(_*_)
      // return result, and remap outputs of the function to the corresponding fresh names
      (sbRmIns * argsComp,sbOuts.map(remap),sbCtx)
    case _ => throw new RuntimeException(s"Stream Expression ${se} of type ${se.getClass} not supported.")
    // todo: Any kind of StreamFun if we go back to having this option,
    //    but if will required sequence of inputs as well.
  }

  private def mkBuild(qs:List[ConstEntry]
                      , args:List[TGroundTerm]
                      , sbCtx:SBContext): SemanticResult = {
    val out = freshVar()
    val (gcs,sbs) = mkGCBuild(qs,args,sbCtx,out)
    val buildSb = sb withCommands (gcs:_*) outs out ins (gcs.flatMap(gc=>gc.inputs):_*)
    (buildSb*sbs,List(out),sbCtx)
  }

  private def mkGCBuild(qs:List[ConstEntry]
                     , args:List[TGroundTerm]
                     , sbCtx:SBContext,out:String): (List[GuardedCommand],StreamBuilder) = qs match {
    case Nil => (List(),DSL.sb)
    case q::more =>
      // get the arguments that correspond to the first constructor q
      val numArgs = q.params.size
      val qArgs = args.take(if (numArgs==0) 1 else numArgs)
      // make a stream builder for each argument that is a term
      //val sbArgs:List[SemanticResult] = qArgs.map(a=> encode(a,sbCtx))
      val sbArgs:List[Either[SemanticResult,Var]]  = qArgs.map(a=> { a match {
        case a:Var => Right(a)
        case _ => Left(encode(a,sbCtx))
      }})
      // make gets for the output variable of each sb in the previous step
      val getArgs = Guard(sbArgs.map(r => if (r.isLeft) r.left.get._2.head else r.right.get.name)
        .map(p => Get(p)).toSet)
      // remove these arguments from the list
      val restArgs = args.drop(if (numArgs==0) 1 else numArgs)
      // build the constructor
      val argsNames = sbArgs.map(r=> if (r.isLeft) r.left.get._2.head else r.right.get.name)
      val constructor = Const(q.name,if (numArgs==0) List() else argsNames.map(p=> Port(p)))
      // composed arguments stream builders into a single stream builder
      val compArgs =  sbArgs.filter(_.isLeft).map(_.left.get._1).foldRight[StreamBuilder](DSL.sb)(_*_)
      // get the rest of the guarded commands
      val gcMore = mkGCBuild(more,restArgs,sbCtx,out)
      // make the actual guarded command for this constructor
      val gcq = getArgs -> (out := toTerm(constructor))
      (gcq::gcMore._1,compArgs*gcMore._2)
  }

  private def mkMatch(qs:List[ConstEntry]
                    , arg:TGroundTerm
                    , sbCtx:SBContext): SemanticResult = {
    val (sbIn,in,_) = arg match {
      case a:Var => (StreamBuilder.empty,List(a.name),sbCtx)
      case _ => encode(arg,sbCtx)}
    val (gcs,sbs,outs) = mkGCMatch(qs,sbCtx,in.head)
    val buildSb = sb withCommands (gcs:_*) outs (outs:_*) ins in.head
    (buildSb*sbs*sbIn,outs,sbCtx)
  }

  private def mkGCMatch(qs:List[ConstEntry]
                        , sbCtx:SBContext,in:String): (List[GuardedCommand],StreamBuilder,List[String]) = qs match {
    case Nil => (List(),DSL.sb,List())
    case q::more =>
      // get the arguments that correspond to the first constructor q
      val numArgs = q.params.size
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
    case TConst(q,_,targs) => targs.flatMap(fv).toSet
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

  private def rename(gc:GuardedCommand,remap:Map[String,String]):GuardedCommand =
    GuardedCommand(rename(gc.guard,remap),gc.cmd.map(c=>rename(c,remap)))

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

  private def rename(c:Command,remap:Map[String,String]):Command =
    Command(remap.getOrElse(c.variable,c.variable),rename(c.term,remap))

  private def rename(t:Term,remap:Map[String,String]):Term = t match {
    case Var(x) => Var(remap.getOrElse(x,x))
    case Q(name,args) => Q(name,args.map(a=>rename(a,remap)))
    case GetQ(name,index,arg) => GetQ(name,index,rename(arg,remap))
  }

  private def toTerm(gt:GroundTerm):Term = gt match {
    case Port(x) => Var(x)
    case Const(name,args) => Q(name,args.map(a=>toTerm(a)))
  }
}
