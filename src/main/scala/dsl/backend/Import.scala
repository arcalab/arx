package dsl.backend


import dsl.analysis.semantics.StreamBuilder.StreamBuilderEntry
import dsl.analysis.syntax.{FunDef, GroundTerm, TypeDecl}

import scala.util.parsing.input.Positional

/**
  * Created by guillecledou on 2020-01-24
  */


case class Import(module:String,members:List[String]) extends Positional


case class Module(name:String,childs:List[Module],content:List[ModuleContent])

sealed trait ModuleContent {
  val name:String
}

//case class PrimFun(name:String,ins:Int,outs:Int) extends ModuleContent
case class PrimFun(name:String,sb:StreamBuilderEntry,params:List[String]=List()) extends ModuleContent
case class ComplFun(name:String,definition:FunDef) extends ModuleContent
case class PrimType(name:String,declaration:TypeDecl) extends ModuleContent