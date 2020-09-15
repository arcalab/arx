package dsl.analysis.types

import dsl.analysis.types.Context._
import dsl.backend.PortType
import dsl.common.SymbolException

import scala.util.parsing.input.Positional


/**
  * Created by guillecledou on 2019-06-19
  */

case class Context(types:TypeCtx
                   , constructors:ConstructorCtx
                   , functions:FunCtx
                   , ports:PortCtx
                   , variables:VarCtx) {

  def hasType(name:String):Boolean = types.contains(name)
  def hasConstructor(name:String):Boolean = constructors.contains(name)
  def hasFun(name:String):Boolean = types.contains(name)
  def hasPort(name:String):Boolean = ports.contains(name)
  def hasVar(name:String):Boolean = variables.contains(name)


  def add(name:String, entry:ContextEntry):Context = entry match {
    case t@TypeEntry(_,_) if !types.contains(name) =>
      Context(types + (name->t),constructors,functions,ports,variables)
    case TypeEntry(_,_)  =>
      symbolException(name,types(name))
    case c@ConstEntry(_,_,_) if !constructors.contains(name) =>
      Context(types,constructors + (name->c),functions,ports,variables)
    case ConstEntry(_,_,_) =>
      symbolException(name,constructors(name))
    case f@FunEntry(_,_,_) if !functions.contains(name) =>
      Context(types,constructors,functions + (name->f),ports,variables)
    case FunEntry(_,_,_) =>
      symbolException(name,functions(name))
    case p@PortEntry(_,_) if !variables.contains(name) =>
      val entries = ports.getOrElse(name,List()):+p
      Context(types,constructors,functions, ports + (name -> entries),variables)
    case PortEntry(_,_) =>
      symbolException(name,variables(name))
    case v@VarEntry(_) if !ports.contains(name) && !variables.contains(name) =>
      Context(types, constructors,functions,ports,variables + (name->v))
    case VarEntry(_) =>
      symbolException(name,if (ports.isDefinedAt(name)) ports(name).head else variables(name))
  }

  def getType(name:String):TypeEntry = types(name)
  def getConst(name:String):ConstEntry = constructors(name)
  def getFun(name:String):FunEntry = functions(name)
  def getPort(name:String):List[PortEntry] = ports(name)
  def getVar(name:String):VarEntry = variables(name)

  private def symbolException(s:String, entry:ContextEntry) =
    throw new SymbolException(s"Symbol $s already found in context in ${entry.pos}")
}

object Context {
  type FunCtx = Map[String,FunEntry]
  type ConstructorCtx = Map[String,ConstEntry]
  type PortCtx = Map[String,List[PortEntry]]
  type VarCtx = Map[String,VarEntry]
  type TypeCtx = Map[String,TypeEntry]

  def apply():Context =
    Context(Map(),Map(),Map(),Map(),Map())

  def newScope(ctx:Context):Context =
    Context(ctx.types,ctx.constructors,ctx.functions,Map(),Map())
}


sealed trait ContextEntry extends Positional {val tExp:TExp}

case class FunEntry(tExp: TFun, ctx: Context, dataParamsType: List[TExp] = List()) extends ContextEntry
case class TypeEntry(constructors:List[String], tExp:TBase) extends ContextEntry
case class ConstEntry(name:String, paramsType:List[TExp], tExp:TExp) extends ContextEntry
case class PortEntry(io:PortType, tExp:TExp) extends ContextEntry
case class VarEntry(tExp:TExp) extends ContextEntry



