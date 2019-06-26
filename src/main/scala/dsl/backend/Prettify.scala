package dsl.backend

import dsl.analysis.semantics.TypeConn
import dsl.analysis.semantics._

/**
  * Created by guillecledou on 2019-06-19
  */


object Prettify {

  /* current number of variables renamed */
  private var seed = 0
  /* Variable free name -> pretty name */
  private var prettyVars:Map[String,String] = Map()

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
    * Given a type expression with free variables, rename all free variables to alphabet letters, accordingly.
    * @param te type expression
    * @return prettified type expression
    */
  def apply(te: TypeExpr):TypeExpr = te match {
    case t@TVar(n) =>
      //if (n.startsWith("_")) {
      if (n.matches("[0-9]")) {
        if (prettyVars.contains(n))
          TVar(prettyVars(n))
        else {
          var s = intToAlpha(prettifySeed())
          prettyVars+= (n->s)
          TVar(s)
        }
      } else t
    case TMap(t1, t2) => TMap(apply(t1), apply(t2))
    case TOpt(t) => TOpt(apply(t))
    case TProd(t, ts) => TProd(apply(t), ts.map(apply))
    case TEithers(t, ts) => TEithers(apply(t), ts.map(apply))
    case TTuple(t, ts) => TTuple(apply(t), ts.map(apply))
    case BaseType(n,ps) => BaseType(n,ps.map(apply))
    case t => t
  }

  /**
    * Given an integer, return the corresponding alphabet letter or sequence of letters.
    * For example: 0 -> "a", 26 -> "z", 27 -> "aa", etc.
    * @param i integer number
    * @return corresponding alphabet letter or sequence of letters
    */
  private def intToAlpha(i:Int):String = {
    val quotient = i/26
    val remainder = i%26
    val res = ('a'.toInt + remainder).toChar
    if (quotient == 0 )
      res.toString
    else intToAlpha(quotient-1)+res
  }
}
