package dsl.backend

import dsl.DSL
import dsl.DSL._
import dsl.analysis.semantics.StreamBuilder.StreamBuilderEntry
import dsl.analysis.semantics.{Q, StreamBuilder, Var}
import dsl.backend.ArxNet.Edge
import dsl.common.UndefinedNameException


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

  private lazy val alltypes =
    """data List<a> = Nil | Cons(a,List<a>)
      |data Bool = True | False
      |data Nat = Zero | Succ(Nat)
      |data Pair<a,b> = P(a,b)
      |data Either<a,b> = Left(a) | Right(b)
      |data Unit = U"""

  // all primitive types indexed by name
  private lazy val types:Map[String,PrimType] =
    mkTypes(List(unit,nat,list,bool,pair,either).mkString("\n"))

  // return a list of type declarations for all primitive types
  def importPrimTypes():List[PrimType] = types.values.toList

  // return the type declaration for type @name if known
  def importType(name:String):PrimType =
    if (types.contains(name)) types(name)
    else throw new UndefinedNameException(s"Unknown type $name")

  // generate type declarations for primitive types
  private def mkTypes(typesDef:String):Map[String,PrimType] = {
    val program = DSL.parse(typesDef)
    program.types.map(t=> t.name.name->PrimType(t.name.name,t)).toMap
  }

   /* FUNCTIONS */

  /* Stream builders for primitive function  */

  private lazy val fifosb = (sb withCommands (
    (get("in") & und("m"))->("m":= Var("in")),
    get("m") -> ("out":= Var("m"))
  ) ins "in" outs "out" mems "m", List("in"),List("out"))

  private lazy val fifoFullsb = (fifosb._1 initially ("m" := Var("p1")),fifosb._2,fifosb._3)
  //private lazy val fifoFull0sb = (fifosb._1 initially ("m" := Q("Zero",List())),fifosb._2,fifosb._3)

  private lazy val idsb = (sb withCommands (
    get("in")->("out":=Var("in"))
  ) ins "in" outs "out",List("in"),List("out"))

  private lazy val syncsb = idsb

  private lazy val lossysb = (sb withCommands (
    get("in")->("out":=Var("in")),
    get("in")->()
  ) ins "in" outs "out",List("in"),List("out"))

  private lazy val duplsb = (sb withCommands (
    get("in") -> ("out1":=Var("in"),"out2":= Var("in"))
  ) ins "in" outs("out1","out2"),List("in"),List("out1","out2"))

  private lazy val mergersb = (sb withCommands (
    get("in1") -> ("out":= Var("in1")),
    get("in2") -> ("out":= Var("in2"))
  ) ins ("in1","in2") outs "out", List("in1","in2"),List("out"))

  private lazy val xorsb = (sb withCommands (
    get("in") -> ("out1":=Var("in")),
    get("in") -> ("out2":= Var("in"))
  ) ins "in" outs("out1","out2"),List("in"),List("out1","out2"))

  private lazy val drainsb = (sb withCommands (
    (get("in1") & get("in2")) -> ()
  ) ins ("in1","in2"),List("in1","in2"),List())

  private lazy val writersb = (sb initially ("m" := Var("p1")) withCommands (
      get("m") -> ("out":= Var("m"))
  ) outs "out" mems "m", List(),List("out"))

  private lazy val readersb = (sb withCommands (
      get("in") -> ("m":= Var("in"))
    ) ins "in" mems "m",List("in"),List())

  private lazy val nowritersb = (sb outs "out", List(), List("out"))
  private lazy val noreadersb = (sb ins "in", List("in"), List())

  /* Signature for primitive functions */

//  private lazy val fifo = PrimFun("fifo",1,1)
//  private lazy val fifofull = PrimFun("fifofull",1,1)
//  private lazy val lossy = PrimFun("lossy",1,1)
//  private lazy val sync = PrimFun("sync",1,1)
//  private lazy val id = PrimFun("id",1,1)
//  private lazy val dupl = PrimFun("dupl",1,2)
//  private lazy val xor = PrimFun("xor",1,2)
//  private lazy val merger = PrimFun("merger",2,1)
//  private lazy val drain = PrimFun("drain",2,0)
//  private lazy val writer = PrimFun("writer",0,1)
//  private lazy val reader = PrimFun("reader",1,0)

  private lazy val fifo = mkPrimFun("fifo",fifosb)
  private lazy val fifofull = mkPrimFun("fifofull",fifoFullsb,List("p1"))
  //private lazy val fifofull0 = PrimFun("fifofull0",fifoFull0sb,List("p1"))
  private lazy val lossy = mkPrimFun("lossy",lossysb)
  private lazy val sync = mkPrimFun("sync",syncsb)
  private lazy val id = mkPrimFun("id",idsb)
  private lazy val dupl = mkPrimFun("dupl",duplsb)
  private lazy val xor = mkPrimFun("xor",xorsb)
  private lazy val merger = mkPrimFun("merger",mergersb)
  private lazy val drain = mkPrimFun("drain",drainsb)
  private lazy val writer = mkPrimFun("writer",writersb)
  private lazy val reader = mkPrimFun("reader",readersb)
  private lazy val nowriter = mkPrimFun("nowriter", nowritersb)
  private lazy val noreader = mkPrimFun("noreader", noreadersb)

  def mkPrimFun(name:String,sb:(StreamBuilder,List[String],List[String]),params:List[String]=List()): PrimFun = {
    val net = new ArxNet += Edge(sb._2.toSet,sb._3.toSet,name)
    PrimFun(name,(sb._1,sb._2,sb._3,net),params)
  }

  private lazy val functions :Map[String,PrimFun] =
    List(fifo,fifofull,lossy,sync,id,dupl,xor,merger,drain,writer,reader,nowriter,noreader)
    .map(f=> f.name -> f).toMap

  // return the function type for function @name if known
  def importPrimFun(name:String):PrimFun =
    if (functions.contains(name)) functions(name)
    else throw new UndefinedNameException(s"Unknown function $name")

  // returns a list of function types for all primitive functions
  def importPrimFunctions():List[PrimFun] = functions.values.toList

  def primitiveFunctionNames():Set[String] = functions.keySet
  // Complex functions

  private lazy val counter =
    """def counter(tick): Nat = {
      |    drain(tick,n)
      |    succ<-build(U,n) // build<Nat>
      |    next<-fifo(succ)
      |    iter<-fifofull(next) // filled with Zero
      |    n,res<-xor(iter)
      |    zero<-Zero
      |    drain(res,zero)
      |    succ<-zero
      |    res
      |  }
    """.stripMargin

  private lazy val alt =
    """def alt(i1,i2) = {
       |  a<-in1(i1) b<-in2(i2)
       |  drain(a, b)
       |  x<-a x<-fifo(b)
       |  out(x)
       |}
       |alt(x,y)"""

  private lazy val lossyfifo =
    """y<-lossy(x)
      |fifo(y)"""

  private lazy val miscdata =
    """data List<a> = Nil | Cons(a,List<a>)
      |data Bool = True | False
      |data Nat = Zero | Succ(Nat)
      |data Pair<a,b> = P(a,b)
      |data Either<a,b> = Left(a) | Right(b)
      |data Unit = U
      |
      |x <- Cons(Zero,Nil)
      |y <- Cons(Zero,x)
      |z <- Cons(Succ(Succ(Zero)),y)
      |w <- True
      |a,b,c <- dupl3(x)
      |
      |def alt(i1,i2) = {
      |  a<-in1(i1) b<-in2(i2)
      |  drain(a, b)
      |  o<-a o<-fifo(b)
      |  o
      |}
      |// If Then Else
      |def ite(b:Bool,then:A,else:A): A = {
      |    t,f <- match(b)
      |    drain(t,ok)
      |    drain(f,ko)
      |    ok
      |    ko
      |}
      |
      |// fibbonaci
      |def fib(): Nat = {
      |  b<-fifoFull_Succ_Zero(a)
      |  c<-fifo(b)
      |  a <- add(b,c)
      |  a
      |}
      |
      |// counts ticks (to check)
      |def counter(tick): Nat = {
      |  drain(tick,n)
      |  succ<-build(nil,n)
      |  next<-fifo(succ)
      |  iter<-fifoFull_Zero(next)
      |  n,res<-xor(iter)
      |  zero<-Zero
      |  drain(res,zero)
      |  succ<-zero
      |  res
      |}
      |
      |// Addition of naturals (to check)
      |def add(a, b): Nat = {
      |  drain(a,b)
      |  lockAll<-fifo(a)
      |  lockA<-fifo(a)
      |  waitB<-fifo(b)
      |  next<-a
      |  toMatch<-fifo(next)
      |  zero,succ<-match(toMatch)
      |  next<-fifo(succ)
      |  res<-counter(succ)
      |  aDone,bDone<-xor(zero)
      |  drain(aDone,lockA)
      |  drain(aDone,waitB)
      |  next<-waitB
      |  lockB<-fito(waitB)
      |  drain(bDone,lockB)
      |  drain(bDone,lockAll)
      |  drain(bDone,res)
      |  res
      |}"""

  private lazy val sequencer3 =
    """x1<-fifofull(x3) drain(o1,x1) out1(o1)
      |x2<-    fifo(x1) drain(o2,x2) out2(o2)
      |x3<-    fifo(x2) drain(o3,x3) out3(o3)
      |"""


  private lazy val programs =
    Map("counter"->counter,
      "alt"->alt,
      "lossyfifo"->lossyfifo,
      "miscdata"->miscdata,
      "sequencer3"->sequencer3)

  private lazy val mathFunctions = Map("counter"->counter)

  def loadComplexFunctions():List[ComplFun] =
    mathFunctions.map(f=> ComplFun(f._1,DSL.parseFunction(f._2))).toList

  def getImport(i:Import):List[ModuleContent] =
    findModule(i.module.split("\\.").toList,modules) match {
      case Some(module) =>
        if (i.members.isEmpty) getAllContent(module)
        else getMembers(module,i.members)
      case None =>
        if (i.members.isEmpty) findModule(i.module.split("\\.").toList.init,modules) match {
          case Some(module) => getMembers(module,List(i.module.split("\\.").last))
          case None => throw new UndefinedNameException(s"Unknown module name ${i.module}")
        } else throw new UndefinedNameException(s"Unknown module name ${i.module}")
    }

  @scala.annotation.tailrec
  private def findModule(names:List[String], namespace:Map[String,Module]):Option[Module] = names match {
    case Nil => None
    case n :: Nil if namespace.contains(n) => Some(namespace(n))
    case n :: ns if namespace.contains(n) =>
      val nNameSpace = namespace(n).childs.map(m => m.name -> m).toMap
      findModule(ns, nNameSpace)
    case _ => None
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

  private lazy val typesMod = Module("Types",List(),importPrimTypes())
  private lazy val connsMod = Module("Conn",List(primitiveMod,mathMod),List())
  private lazy val primitiveMod = Module("Prim",List(),importPrimFunctions())
  private lazy val mathMod = Module("Math",List(),loadComplexFunctions())
//
  private lazy val modules:Map[String,Module] = List(typesMod,connsMod).map(m=>m.name->m).toMap

}

