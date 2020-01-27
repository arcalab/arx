package dsl.backend

import dsl.analysis.syntax.{FunDef, TypeDecl}

/**
  * Created by guillecledou on 2020-01-24
  */


case class Import(module:String,members:List[String])


case class Module(name:String,childs:List[Module],content:List[ModuleContent])

sealed trait ModuleContent {
  val name:String
}

case class PrimFun(name:String,ins:Int,outs:Int) extends ModuleContent
case class ComplFun(name:String,definition:FunDef) extends ModuleContent
case class PrimType(name:String,declaration:TypeDecl) extends ModuleContent