package dsl.analysis.semantics

/**
  * Created by guillecledou on 2019-06-19
  */


class Context {
  protected val ctx: Map[String,TVar] = Map()

  def get:Map[String,TVar] = ctx

  def contains(x:String):Boolean = ctx contains x

  def apply(x:String):TVar = ctx(x)

  // todo: handle multiple definitions of a variable, for now assume it doesn't happen
  def add(x:String,t:TVar):Context = {
    var oldCtx = ctx
    new Context {
      override val ctx = oldCtx + (x -> t)
    }
  }
  // todo: handle multiple definitions of a variable, for now we don't have different scopes
  def join(other:Context):Context = {
    var oldCtx = ctx
    var newCon:Context = other
    for ((k,v) <- oldCtx) {
      newCon = newCon.add(k,v)
    }
    newCon
  }
}
