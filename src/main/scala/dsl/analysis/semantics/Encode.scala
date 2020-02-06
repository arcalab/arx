package dsl.analysis.semantics

import dsl.DSL
import dsl.DSL._
import dsl.analysis.semantics.StreamBuilder.StreamBuilderEntry
import dsl.analysis.syntax._
import dsl.analysis.types.Context

/**
  * Created by guillecledou on 2020-02-06
  */


object Encode{

  type SemanticEncoding = (StreamBuilder,List[String])

  private var vars:Int = 0
  private def freshVar():String = {vars+=1; (vars-1).toString}

  def apply(program:Program,typedCtx:Context):SemanticEncoding = {
    // create context with primitive functions
    val ctx = loadPrimitives()
    //todo: continue
    (DSL.sb,List())
  }

  /**
    * Generates the stream builder of a statement
    * @param st
    * @return stream builder fo st
    */
  private def encode(st:Statement, sbCtx:SBContext, typeCtx:Context):SemanticEncoding = st match {
    case se:StreamExpr => encode(se,sbCtx,typeCtx)
    case Assignment(variables, expr) =>
      //todo: continue
      (DSL.sb,List())
    case FunDef(name, params, typ, block) =>
      //todo: continue
      (DSL.sb,List())
    //todo: case for SFunDef
  }

  /**
    * Generates the stream builder of a stream expression
    * @param se
    * @return stream builder for se
    */
  private def encode(se:StreamExpr, sbCtx:SBContext, typeCtx:Context):SemanticEncoding = se match {
    case Port(x) =>
      var out = freshVar()
      var gc  = Get(x) -> (out := Port(x))
      (StreamBuilder(Set(),Set(gc),Set(x),Set(out)),List(out))
    case q@Const(name,args) =>
      var fvq = fv(q)
      var gets = fvq.map(Get).foldRight[Guard](True)(_&_)
      var out = freshVar()
      var gc = gets -> (out := q)
      (StreamBuilder(Set(),Set(gc),fvq,Set(out)),List(out))
    case FunctionApp(Match, args) =>
      //todo: continue
      (DSL.sb,List())
    case FunctionApp(Build, args) =>
      //todo: continue
      (DSL.sb,List())
    case FunctionApp(FunName(name), args) =>
      // get the stream builder entry associated to name
      val sbe = sbCtx(name)
      // get a stream builder for each argument
      val argsSb:List[SemanticEncoding]  = args.map(a=> encode(a,sbCtx,typeCtx))
      // zip inputs with the output of the corresponding argument (we know they have only 1 output)
      val remapInputs:List[(String,String)] = sbe._2.zip(argsSb.map(_._2.head))
      // fresh variables new inputs correspond with args, new outputs and memories are fresh variables
      val remap  = (remapInputs ++
        sbe._2.map(o=> (o,freshVar())) ++
        sbe._1.memory.map(m=> (m,freshVar()))).toMap
      // get a fresh instantiation of the stream builder based on the new name mapping
      val freshSb = fresh(sbe,remap)
      // compose stream builders from arguments
      val argsComp = argsSb.map(_._1).foldRight[StreamBuilder](DSL.sb)(_*_)
      // return result
      (freshSb._1 * argsComp,freshSb._2)
    // todo: eventually if we go back to having this option,
    //  but if will required sequence of inputs as well.
  }

//  /**
//    * Generates the stream builder of a stream function
//    * @param sf
//    * @return
//    */
//  private def encode(sf:StreamFun):SemanticEncoding = sf match {
//    case FunName(f) =>
//    case ParFun(f1, f2) =>
//    case SeqFun(f1, f2) =>
//    case Match =>
//    case Build =>
//  }


  private def loadPrimitives():SBContext = {
    val primitives = DSL.prelude.importPrimFunctions()
    var ctx = SBContext()
    for (f <- primitives) {
      ctx = ctx.add(f.name, f.sb)
    }
    ctx
  }

  private def fv(gt:GroundTerm):Set[String] = gt match {
    case Port(x) => Set(x)
    case Const(q,args) => args.flatMap(fv).toSet
  }

  private def fresh(sbe:StreamBuilderEntry,remap:Map[String,String]):StreamBuilderEntry = {
    val sb = sbe._1
    //val variables = sb.outputs++sb.inputs++sb.outputs
    //todo check if the sets can overlap, then do smart rename to remember new known names
    //val remap:Map[String,String] = variables.map(v => v->freshVar()).toMap

    val ins  = sb.inputs.map(v=>remap(v))
    val outs = sb.outputs.map(v=>remap(v))
    val mems = sb.memory.map(v=>remap(v))
    val gcs  = sb.gcs.map(gc=>rename(gc,remap))
    val init = sb.init.map(c=>rename(c,remap))

    (StreamBuilder(init,gcs,ins,outs,mems),sbe._2.map(remap),sbe._3.map(remap))
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
    Command(remap(c.variable),rename(c.term,remap))

  private def rename(t:GroundTerm,remap:Map[String,String]):GroundTerm = t match {
    case Port(x) => Port(remap(x))
    case Const(name,args) => Const(name,args.map(a=>rename(a,remap)))
  }
}
