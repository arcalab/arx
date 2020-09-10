package dsl.backend

import dsl.analysis.semantics.{SBContext, StreamBuilder}
import dsl.analysis.semantics.StreamBuilder.StreamBuilderEntry
import dsl.analysis.types.TProgram.TBlock
import dsl.analysis.types._

/**
  * Created by guillecledou on 2019-06-19
  */


object Prettify {

  /* current number of variables renamed */
  private var seed = 0
  /* Variable free name -> pretty name */
  private var prettyVars:Map[String,String] = Map()


  /**
    * Prettifies a context by replacing type variables with alphabet letters, accordingly
    * @param ctx a type context
    * @return prettified context
    */
  def apply(ctx:Context):Context = {
    val nFuns = ctx.functions.map(f=> f._1 ->
      FunEntry(TFun(apply(f._2.tExp.tIn),apply(f._2.tExp.tOut)),apply(f._2.funCtx)))
    val nPorts =
      ctx.ports.map(p => p._1 -> p._2.map(apply))
    val nVars = ctx.vars.map(m=> m._1 -> VarEntry(apply(m._2.tExp)))
    Context(ctx.adts,nFuns,nPorts,nVars)
  }

  def apply(p:TProgram):TProgram = TProgram(p.imports,p.userTypes,apply(p.tBlock))

  def apply(tb:TBlock):TBlock = tb.map(apply)

  def apply(s:TStatement):TStatement = s match {
    case TFunDef(f,t,tb) => TFunDef(f,apply(t),apply(tb))
    case TSBDef(sb,t) => TSBDef(sb,apply(t))
    case TSFunDef(f,t,tb) => TSFunDef(f,apply(t),apply(tb))
    case TAssignment(a,tlhs,trhs) => TAssignment(a,tlhs.map(apply),apply(trhs))
    case TRAssignment(a,tlhs,trhs) => TRAssignment(a,tlhs.map(apply),apply(trhs))
    case se:TStreamExpr => apply(se)
  }

  def apply(tsf:TStreamFun):TStreamFun = tsf match {
    case TFunName(f,t,d)  => TFunName(f,apply(t),d.map(apply))
    case TBuild(t,ta)   => TBuild(apply(t),apply(ta))
    case TMatch(t,ta)   => TMatch(apply(t),apply(ta))
    case TSeqFun(t1,t2) => TSeqFun(apply(t1),apply(t2))
    case TParFun(t1,t2) => TParFun(apply(t1),apply(t2))
  }

  def apply(tse:TStreamExpr):TStreamExpr = tse match {
    case TFunApp(sf,t,ta) => TFunApp(apply(sf),apply(t),ta.map(apply))
    case tgt:TGroundTerm => apply(tgt)
  }

  def apply(tgt:TGroundTerm):TGroundTerm = tgt match {
    case TConst(q, t, tArgs) => TConst(q, apply(t), tArgs.map(apply))
    case TPort(p, t) => TPort(p,apply(t))
  }

  /**
    * Given a type expression with free variables, rename all free variables to alphabet letters, accordingly.
    * @param te type expression
    * @return prettified type expression
    */
  def apply(te: TExp):TExp = te match {
    case TVar(n) if n.matches("[0-9]*") => TVar(prettify(n))
    case t@TVar(_) => t
    case TTensor(t1,t2) => TTensor(apply(t1),apply(t2))
    case TFun(i,o) => TFun(apply(i)/*.asInstanceOf[TInterface]*/,apply(o)/*.asInstanceOf[TInterface]*/)
    case TBase(n,ps) => TBase(n,ps.map(apply))
    case t => t
  }

  def prettify(str:String) =
    if (prettyVars.contains(str))
      prettyVars(str)
    else {
      val s = intToAlpha(prettifySeed())
      prettyVars+= (str->s)
      s
    }


  /**
    * Return current seed and increased seeds by one
    * @return current seed
    */
  def reset() = {
    seed = 0
    prettyVars = Map()
  }

  /**
    * Return current seed and increased seeds by one
    * @return current seed
    */
  private def prettifySeed():Int = {seed+=1; seed-1}

  /**
    * Given an integer, return the corresponding alphabet letter or sequence of letters.
    * For example: 0 -> "a", 26 -> "z", 27 -> "aa", etc.
    * @param i integer number
    * @return corresponding alphabet letter or sequence of letters
    */
  def intToAlpha(i:Int):String = {
    val quotient = i/26
    val remainder = i%26
    val res = ('a'.toInt + remainder).toChar
    if (quotient == 0 )
      res.toString
    else intToAlpha(quotient-1)+res
  }

  /**
    * Prettifies a port entry by replacing type variables with alphabet letters, accordingly
    * @param pe port entry
    * @return prettified port entry
    */
  private def apply(pe:PortEntry):PortEntry = PortEntry(apply(pe.tExp),pe.pType)
}
