package dsl.backend

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
    Context(ctx.adts,nFuns,nPorts)
  }


  /**
    * Given a type expression with free variables, rename all free variables to alphabet letters, accordingly.
    * @param te type expression
    * @return prettified type expression
    */
  def apply(te: TExp):TExp = te match {
    case TVar(n) if n.matches("[0-9]*") =>
      if (prettyVars.contains(n)) TVar(prettyVars(n))
      else {
        var s = intToAlpha(prettifySeed())
        prettyVars+= (n->s)
        TVar(s)
      }
    case t@TVar(_) => t
    case TTensor(t1,t2) => TTensor(apply(t1),apply(t2))
    case TFun(i,o) => TFun(apply(i)/*.asInstanceOf[TInterface]*/,apply(o)/*.asInstanceOf[TInterface]*/)
    case TBase(n,ps) => TBase(n,ps.map(apply))
    case t => t
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
