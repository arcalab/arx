package dsl.backend

import dsl.DSL
import dsl.analysis.syntax.{FunDef, TypeDecl}
import dsl.common.UndefinedNameException
import spire.syntax.module



/**
  * Created by guillecledou on 2020-01-09
  */


object Prelude {

  // TYPES

  private lazy val unit = "data Unit = U"
  private lazy val nat = "data Nat = Zero | Succ(Nat)"
  private lazy val list = "data List<a> = Nil | Cons(a,List<a>)"
  private lazy val bool = "data Bool = True | False"
  private lazy val pair = "data Pair<a,b> = P(a,b)"
  private lazy val either = "data Either<a,b> = Left(a) | Right(b)"

  // all primitive types indexed by name
  private lazy val types:Map[String,PrimType] = mkTypes(List(unit,nat,list,bool,pair,either).mkString("\n"))

  // return a list of type declarations for all primitive types
  def importTypes():List[PrimType] = types.map(t=>t._2).toList

  // return the type declaration for type @name if known
  def importType(name:String):PrimType =
    if (types.contains(name)) types(name)
    else throw new UndefinedNameException(s"Unknown type ${name}")

  // generate type declarations for primitive types
  private def mkTypes(typesDef:String):Map[String,PrimType] = {
    val program = DSL.parse(typesDef)
    program.types.map(t=> t.name.name->PrimType(t.name.name,t)).toMap
  }

  // FUNCTIONS

  //primitives

  private lazy val fifo = PrimFun("fifo",1,1)
  private lazy val fifofull = PrimFun("fifofull",1,1)
  private lazy val lossy = PrimFun("lossy",1,1)
  private lazy val sync = PrimFun("sync",1,1)
  private lazy val id = PrimFun("id",1,1)
  private lazy val dupl = PrimFun("dupl",1,2)
  private lazy val xor = PrimFun("xor",1,2)
  private lazy val merger = PrimFun("merger",2,1)
  private lazy val drain = PrimFun("drain",2,0)
  private lazy val writer = PrimFun("writer",0,1)
  private lazy val reader = PrimFun("reader",1,0)

  private lazy val functions :Map[String,PrimFun] = List(fifo,fifofull,lossy,sync,id,dupl,xor,merger,drain,writer,reader)
    .map(f=> f.name -> f).toMap

  // return the function type for function @name if known
  def importPrimFun(name:String):PrimFun =
    if (functions.contains(name)) functions(name)
    else throw new UndefinedNameException(s"Unknown function ${name}")

  // returns a list of function types for all primitive functions
  def importPrimFunctions():List[PrimFun] = functions.map(f=>f._2).toList

  // Complex functions

  private lazy val counter =
    """def counter(tick): Nat = {
      |    drain(tick,n)
      |    succ:=build(U,n) // build<Nat>
      |    next:=fifo(succ)
      |    iter:=fifofull(next) // filled with Zero
      |    n,res:=xor(iter)
      |    zero:=Zero
      |    drain(res,zero)
      |    succ:=zero
      |    res
      |  }
    """.stripMargin

  private lazy val mathFunctions = Map("counter"->counter)

  def loadComplexFunctions():List[ComplFun] =
    mathFunctions.map(f=> ComplFun(f._1,DSL.parseFunction(f._2))).toList

  def getImport(i:Import):List[ModuleContent] =
    findModule(i.module.split("\\.").toList,modules) match {
      case Some(module) =>
        if (i.members.isEmpty) getAllContent(module)
        else getMembers(module,i.members)
      case None => throw new UndefinedNameException(s"Unknown module name ${i.module}")
    }

  private def findModule(names:List[String],namespace:Map[String,Module]):Option[Module] = {
//    println(s"names: ${names}")
    names match {
      case Nil => None
      case n :: Nil if namespace.contains(n) => Some(namespace(n))
      case n :: ns if namespace.contains(n) =>
        val nNameSpace = namespace(n).childs.map(m => m.name -> m).toMap
        findModule(ns, nNameSpace)
      case _ => None
    }
  }

  private def getMembers(module:Module,members:List[String]): List[ModuleContent] = members match {
    case Nil => List()
    case m::ms =>
      val member = module.content.find(c=>c.name==m)
      if (member.isDefined) member.get::getMembers(module,ms)
      else throw new UndefinedNameException(s"Unknown member ${m} of module ${module.name}")
  }

  private def getAllContent(module:Module):List[ModuleContent] =
    module.content++module.childs.flatMap(getAllContent)

  // MODULES

  private lazy val typesMod = Module("types",List(),importTypes())
  private lazy val connsMod = Module("conn",List(primitiveMod),List())
  private lazy val primitiveMod = Module("prim",List(),importPrimFunctions())
  private lazy val mathMod = Module("math",List(),loadComplexFunctions())
//
  private lazy val modules:Map[String,Module] = List(typesMod,connsMod).map(m=>m.name->m).toMap

}


