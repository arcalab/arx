package dsl.analysis.semantics

import dsl.DSL
import dsl.DSL._
import dsl.analysis.semantics.StreamBuilder.StreamBuilderEntry
import dsl.analysis.syntax.Program.Block
import dsl.analysis.syntax._
import dsl.analysis.types.TypedProgram.TypedBlock
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

  def apply(program:TypedProgram,typeCtx:Context):SemanticResult = {
    // create context with primitive functions
    val ctx = loadPrimitives()
    // encode program block
    val (sbB,sbBOuts,sbBCtx) = encode(program.typedBlock,ctx,typeCtx)
    (sbB,sbBOuts,sbBCtx)
  }

  /**
    * Generates the stream builder of a block and its corresponding sequence of outputs
    * @param block program block
    * @param sbCtx known context of stream builder functions
    * @param typeCtx known context of types for program elements
    * @return a semantic encoding for block
    */
  private def encode(block:TypedBlock,sbCtx:SBContext,typeCtx:Context):SemanticResult = block match {
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
  private def encode(st:TypedStatement, sbCtx:SBContext,typeCtx:Context):SemanticResult = st match {
    case se:TypedStreamExpr => encode(se,sbCtx,typeCtx)
    case TypedAssignment(asg,lhs, rhs) =>
      // get the stream builder entry of the expression
      val (sbE,sbEOuts,sbECtx) = encode(rhs,sbCtx,typeCtx)
      // create a map from sbEOuts to variables
      val remap = sbEOuts.zip(asg.variables).toMap
      // remap outputs in the sbE to variables
      val sbFresh = fresh(sbE,remap)
      // return fresh stream builder
      (sbFresh,List(),sbECtx)
    case TypedFunDef(fd, te, tb) =>
      // get the stream builder of the block
      val (sbB,sbBOuts,sbBCtx) = encode(tb,sbCtx,typeCtx)
      // create an stream builder entry for the function
      val fEntry = (sbB,fd.params.map(tv=>tv.name),sbBOuts)
      // add the entry to the context returned by encoding the block
      val nSbCtx = sbBCtx.add(fd.name,fEntry)
      // return the new context and an empty stream builder and outputs
      (StreamBuilder.empty,List(),nSbCtx)
    //todo: case for SFunDef
    case _ =>throw new RuntimeException(s"Statement ${st} of type ${st.getClass} not supported.")

  }

  /**
    * Generates the stream builder of a stream expression
    * @param se
    * @return stream builder for se
    */
  private def encode(se:TypedStreamExpr, sbCtx:SBContext,typeCtx:Context):SemanticResult = se match {
    case TypedPort(x,_) =>
      var out = freshVar()
      var gc  = Get(x) -> (out := Port(x))
      (StreamBuilder(Set(),Set(gc),Set(x),Set(out)),List(out),sbCtx)
    case q@TypedConst(const,_,targs) =>
      var fvq = fv(q)
      var gets = fvq.map(Get).foldRight[Guard](True)(_&_)
      var out = freshVar()
      var gc = gets -> (out := const)
      (StreamBuilder(Set(),Set(gc),fvq,Set(out)),List(out),sbCtx)
    case TypedFunApp(TypedMatch(tm,tmargs),_, targs) =>
      //todo: continue
      println(s"Match: ${Show(tmargs)} -> ${Show(tm)} ")
      (DSL.sb,List(),sbCtx)
    case TypedFunApp(TypedBuild(tb@TBase(name,_),tbargs), _, targs) =>
      // for each fv in the arguments get a stream builder
      val fvb:List[String] = targs.flatMap(fv)
      val gets = fvb.map(Get).foldRight[Guard](True)(_&_)
      val constructors:List[ConstEntry] = typeCtx.adts(name).constructors
      val (gcs,sbs,out) = mkGCQs(constructors,targs,sbCtx,typeCtx)
      val buildSb = sb withCommands (gcs:_*) outs out ins (gcs.flatMap(gc=>gc.inputs):_*)
      println(s"Build: ${Show(tbargs)} -> ${Show(tb)} ")
      (buildSb*sbs,List(out),sbCtx)
    case TypedFunApp(TypedFunName(name,_),_, args) =>
      // get the stream builder entry associated to name
      val (sb,sbIns,sbOuts) = if (sbCtx.contains(name)) sbCtx(name) else sbCtx("id") //otherwise, assume 1->1 function
      // get a stream builder for each argument todo: probably just for free variables.
      val argsSb:List[SemanticResult]  = args.map(a=> encode(a,sbCtx,typeCtx))
      // zip inputs with the output of the corresponding argument (we know they have only 1 output)
      val remapInputs:List[(String,String)] = sbIns.zip(argsSb.map(_._2.head))
      // fresh variables new inputs correspond with args, new outputs and memories are fresh variables
      val remap  = (remapInputs ++
        sbOuts.map(o=> (o,freshVar())) ++
        sb.memory.map(m=> (m,freshVar()))).toMap
      // get a fresh instantiation of the stream builder based on the new name mapping
      val sbFresh = fresh(sb,remap)
      // compose stream builders from arguments
      val argsComp = argsSb.map(_._1).foldRight[StreamBuilder](DSL.sb)(_*_)
      // return result, and remap outputs of the function to the corresponding fresh names
      (sbFresh * argsComp,sbOuts.map(remap),sbCtx)
    case _ => throw new RuntimeException(s"Stream Expression ${se} of type ${se.getClass} not supported.")
    // todo: Any kind of StreamFun if we go back to having this option,
    //  but if will required sequence of inputs as well.
  }

  private def mkGCQs(qs:List[ConstEntry]
                     , args:List[TypedGroundTerm]
                     , sbCtx:SBContext
                     , typeCtx:Context): (List[GuardedCommand],StreamBuilder,String) = qs match {
    case Nil => (List(),DSL.sb,"")
    case q::more =>
      // get the arguments that correspond to the first constructor q
      val qArgs = args.take(q.params.size)
      // make a stream builder for each argument
      val sbArgs:List[SemanticResult] = qArgs.map(a=> encode(a,sbCtx,typeCtx))
      // make gets for the output variable of each sb in the previous step
      val getArgs = sbArgs.map(_._2.head).map(p => Get(p)).foldRight[Guard](True)(_&_)
      // remove this arguments from the list
      val restArgs = args.drop(q.params.size)
      // build the constructor
      val constructor = Const(q.name,sbArgs.map(p=> Port(p._2.head)))
      // composed arguments stream builders into a single stream builder
      val compArgs = sbArgs.map(_._1).foldRight[StreamBuilder](DSL.sb)(_*_)
      // get the rest of the guarded commands
      val gcMore = mkGCQs(more,restArgs,sbCtx,typeCtx)
      // make the actual guarded command for this constructor
      val out = freshVar()
      val gcq = getArgs -> (out := constructor)
      (gcq::gcMore._1,compArgs*gcMore._2,out)
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
  private def fv(gt:TypedGroundTerm):Set[String] = gt match {
    case TypedPort(x,_) => Set(x)
    case TypedConst(q,_,targs) => targs.flatMap(fv).toSet
  }

  /**
    * Substitutes a stream builder with fresh names based on a given mapping
    * todo: check if the sets can overlap, then do smart rename to remember new known names
    * @param sbe stream builder entry
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

  private def rename(g:Guard,remap:Map[String,String]):Guard = g match {
    case Get(v) if remap.contains(v) => Get(remap(v))
    case Ask(v) if remap.contains(v) => Ask(remap(v))
    case Und(v) if remap.contains(v) => Und(remap(v))
    case And(g1,g2) => And(rename(g1,remap),rename(g2,remap))
    case _ => g
  }

  private def rename(c:Command,remap:Map[String,String]):Command =
    Command(remap.getOrElse(c.variable,c.variable),rename(c.term,remap))

  private def rename(t:GroundTerm,remap:Map[String,String]):GroundTerm = t match {
    case Port(x) => Port(remap.getOrElse(x,x))
    case Const(name,args) => Const(name,args.map(a=>rename(a,remap)))
  }
}
