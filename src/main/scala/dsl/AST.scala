package dsl

/**
  * Created by guillerminacledou on 2019-05-31
  */


trait AST {}

//case class TypeDecl(name:String,variants:List[AST],param:List[AST]=List()) extends AST {}
case class TypeDecl(name:TypeName,variants:List[AST]) extends AST {}
case class PTypeName(name:String) extends AST{}
case class TypeName(name:String,param:List[AST]=List()) extends AST{}
case class TypeVal(name:String) extends AST {}
case class TypeCons(name:String,param:List[AST]) extends AST {}



