package dsl.backend

import dsl.DSL
import dsl.analysis.syntax.TypeDecl
import dsl.common.UndefinedNameException



/**
  * Created by guillecledou on 2020-01-09
  */


object Prelude {

  // TYPES

  private def unit = "data Unit = U"
  private def nat = "data Nat = Zero | Succ(Nat)"
  private def list = "data List<a> = Nil | Cons(a,List<a>)"
  private def bool = "data Bool = True | False"
  private def pair = "data Pair<a,b> = P(a,b)"
  private def either = "data Either<a,b> = Left(a) | Right(b)"

  // all primitive types indexed by name
  private lazy val types:Map[String,TypeDecl] = mkTypes(List(unit,nat,list,bool,pair,either).mkString("\n"))

  // return a list of type declarations for all primitive types
  def importTypes():List[TypeDecl] = types.map(t=>t._2).toList

  // return the type declaration for type @name if known
  def importType(name:String):TypeDecl =
    if (types.contains(name)) types(name)
    else throw new UndefinedNameException(s"Unknown type ${name}")

  // generate type declarations for primitive types
  private def mkTypes(typesDef:String):Map[String,TypeDecl] = {
    val program = DSL.parse(typesDef)
    program.types.map(t=> t.name.name->t).toMap
  }

  // FUNCTIONS

  private def fifo = PrimFun("fifo",1,1)
  private def fifofull = PrimFun("fifofull",1,1)
  private def lossy = PrimFun("lossy",1,1)
  private def sync = PrimFun("sync",1,1)
  private def id = PrimFun("id",1,1)
  private def dupl = PrimFun("dupl",1,2)
  private def xor = PrimFun("xor",1,2)
  private def merger = PrimFun("merger",2,1)
  private def drain = PrimFun("drain",2,0)
  private def writer = PrimFun("writer",0,1)
  private def reader = PrimFun("reader",1,0)

  private val functions :Map[String,PrimFun] = List(fifo,fifofull,lossy,sync,id,dupl,xor,merger,drain,writer,reader)
    .map(f=> f.name -> f).toMap

  // return the function type for function @name if known
  def importFun(name:String):PrimFun =
    if (functions.contains(name)) functions(name)
    else throw new UndefinedNameException(s"Unknown function ${name}")

  // returns a list of function types for all primitive functions
  def importFunctions():List[PrimFun] = functions.map(f=>f._2).toList

  // MODULES

//  private val typesMod = Module("types")

}


