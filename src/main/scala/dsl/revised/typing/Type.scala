package dsl.revised.typing

import Type._

sealed abstract class Type:
  def vars: Set[String] = this match
    case BaseType(_,args) => args.toSet.flatMap(_.vars)
    case VarType(n) => Set(n)

  override def toString: String = this match
    case BaseType(n,args) => n + (if args.nonEmpty then args.mkString("(",",",")") else "")
    case VarType(n) => n


object Type:
  case class BaseType(name:String, args:List[Type]=Nil) extends Type
  case class VarType(name:String) extends Type

  def replace(ts:List[Type])(using subst:Map[String,Type]): List[Type] =
    ts.map(replace)

  def replace(t:Type)(using m:Map[String,Type]): Type = t match
    case BaseType(n, args) => BaseType(n,replace(args))
    case VarType(n) => m.getOrElse(n,t)


