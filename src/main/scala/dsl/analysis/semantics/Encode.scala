package dsl.analysis.semantics

import dsl.DSL
import dsl.DSL._
import dsl.analysis.semantics.StreamBuilder.StreamBuilderEntry
import dsl.analysis.syntax.Program.Block
import dsl.analysis.syntax._
import dsl.analysis.types.Context
import dsl.backend.Show

/**
  * Created by guillecledou on 2020-02-06
  */


object Encode{

  //type SemanticEncoding = (StreamBuilder,List[String])
  type SemanticResult = (StreamBuilder,List[String],SBContext)

  private var vars:Int = 0
  private def freshVar():String = {vars+=1; s"v${vars-1}"}

  def apply(program:Program,typedCtx:Context):SemanticResult = {
    // create context with primitive functions
    val ctx = loadPrimitives()
    // encode program block
    val (sbB,sbBOuts,sbBCtx) = encode(program.block,ctx,typedCtx)
    (sbB,sbBOuts,sbBCtx)
  }

  /**
    * Generates the stream builder of a block and its corresponding sequence of outputs
    * @param block program block
    * @param sbCtx known context of stream builder functions
    * @param typeCtx known context of types for program elements
    * @return a semantic encoding for block
    */
  private def encode(block:Block,sbCtx:SBContext, typeCtx:Context):SemanticResult = block match {
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
  private def encode(st:Statement, sbCtx:SBContext, typeCtx:Context):SemanticResult = st match {
    case se:StreamExpr => encode(se,sbCtx,typeCtx)
    case Assignment(variables, expr) =>
      // get the stream builder entry of the expression
      val (sbE,sbEOuts,sbECtx) = encode(expr,sbCtx,typeCtx)
      // create a map from sbEOuts to variables
      val remap = sbEOuts.zip(variables).toMap
      // remap outputs in the sbE to variables
      val sbFresh = fresh(sbE,remap)
      // return fresh stream builder 
      (sbFresh,List(),sbECtx)
    case FunDef(name, params, typ, block) =>
      // get the stream builder of the block
      val (sbB,sbBOuts,sbBCtx) = encode(block,sbCtx,typeCtx)
      // create an stream builder entry for the function
      val fEntry = (sbB,params.map(tv=>tv.name),sbBOuts)
      // add the entry to the context returned by encoding the block
      val nSbCtx = sbBCtx.add(name,fEntry)
      // return the new context and an empty stream builder and outputs
      (StreamBuilder.empty,List(),nSbCtx)
    //todo: case for SFunDef
    case _ =>throw new RuntimeException(s"Statement ${Show(st)} of type ${st.getClass} not supported.")

  }

  /**
    * Generates the stream builder of a stream expression
    * @param se
    * @return stream builder for se
    */
  private def encode(se:StreamExpr, sbCtx:SBContext, typeCtx:Context):SemanticResult = se match {
    case Port(x) =>
      var out = freshVar()
      var gc  = Get(x) -> (out := Port(x))
      (StreamBuilder(Set(),Set(gc),Set(x),Set(out)),List(out),sbCtx)
    case q@Const(name,args) =>
      var fvq = fv(q)
      var gets = fvq.map(Get).foldRight[Guard](True)(_&_)
      var out = freshVar()
      var gc = gets -> (out := q)
      (StreamBuilder(Set(),Set(gc),fvq,Set(out)),List(out),sbCtx)
    case FunctionApp(Match, args) =>
      //todo: continue
      (DSL.sb,List(),sbCtx)
    case FunctionApp(Build, args) =>
      //todo: continue
      (DSL.sb,List(),sbCtx)
    case FunctionApp(FunName(name), args) =>
      // get the stream builder entry associated to name
      val (sb,sbIns,sbOuts) = if (sbCtx.contains(name)) sbCtx(name) else sbCtx("id") //otherwise, assume 1->1 function
      // get a stream builder for each argument
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
    case _ => throw new RuntimeException(s"Stream Expression ${Show(se)} of type ${se.getClass} not supported.")
    // todo: Any kind of StreamFun if we go back to having this option,
    //  but if will required sequence of inputs as well.
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
  private def fv(gt:GroundTerm):Set[String] = gt match {
    case Port(x) => Set(x)
    case Const(q,args) => args.flatMap(fv).toSet
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
