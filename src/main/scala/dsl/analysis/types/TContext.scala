package dsl.analysis.types

import dsl.analysis.syntax.SymbolType._
import dsl.backend.{Mix,In,Out,PType}


/**
  * Created by guillecledou on 2019-06-19
  */

case class TContext(adts : Map[String,TypeEntry],
                    functions: Map[String,FunEntry],
                    ports: Map[String,List[PortEntry]]) {

  val constructors: Map[String,ConstEntry] = TContext.mkConstructors(adts)

  val context: Map[String,ContextEntry] = adts++functions++constructors

  //def apply(name:String):Option[ContextEntry] = context.get(name)

  def contains(name:String):Boolean = (context contains name) || (ports contains name)

  def add(name:String,entry:ContextEntry):TContext = entry match {
    case p@PortEntry(_,_) =>
        checkNotIn(name,functions++adts)
        var oldPortEntries:List[PortEntry] = ports.getOrElse(name,List())
        new TContext(adts, functions, ports + (name -> (p::oldPortEntries)))
    case f@FunEntry(_,_)=>
        checkNotIn(name,context)
        new TContext(adts,functions+(name->f),ports)
    case t@TypeEntry(_,_) =>
        checkNotIn(name,context)
        new TContext(adts+(name->t),functions,ports)
    case _ => throw new RuntimeException("Constructors are added automatically when discovering ADT declarations")
  }

  private def checkNotIn(name:String,ctx:Map[String,ContextEntry]):Unit =
    if (ctx.contains(name))
      throw new RuntimeException(s"Name $name already defined in the context")

}

object TContext {

  def apply():TContext = TContext(Map(),Map(),Map())

  def mkConstructors(adts:Map[String,TypeEntry]):Map[String,ConstEntry] = {
    val constructors = adts.flatMap(t => t._2.constructors)
    constructors.map(c => c.name -> c).toMap
  }
}




