package dsl.revised.typing

import dsl.revised
import revised.core._
import revised.typing.MutTypeCtxt._
import revised.typing.Type._
import Connector.{CAut, CNet}
import Network.{Constructor, Link}
import Term._
import dsl.revised.Prelude

// experimenting with mutable class
class MutTypeCtxt(var functions: Map[String,(Types,Type)]=Map(),
                  var connectors: Map[String, () => (Types,Types,Types,MutTypeCtxt)]=Map(),
                  var ports: Map[String,Type]=Map(),
                  var typeConstr: TypeConstraints=Nil,
                  var seed: Int=0):
  def addPort(p:String,t:Type) =
    if ports contains p then
      typeConstr ::=  ports(p) -> t
    else
      ports += p -> t
    this
  def newPort(p:String): Type =
    if ports contains p then ports(p)
    else
      ports += p -> freshVar
      ports(p)
  def addTCons(ts:(Type,Type)) =
    typeConstr ::= ts
    this
  def addTCons(ts:Iterable[(Type,Type)]) =
    typeConstr = typeConstr ++ ts
    this
  def addConn(c:String, getCtx:()=>(List[String],List[String],List[String],MutTypeCtxt)) =
    if connectors contains c then
      dsl.revised.Error.typing(s"connector $c defined twice (known: ${connectors.keys})")
    else
      def inferConTypes() =
        val (args,ins,outs,other) = getCtx()
        def typ(s:String): Type =
          other.ports.getOrElse(s,sys.error(s"Unkown port $s of $c"))
        (args.map(typ),ins.map(typ),outs.map(typ),other)
      connectors += c -> inferConTypes //(inTs.map(typ),outTs.map(typ),other)
      //seed = seed max other.seed
      this

  def freshSubst(vars:Iterable[Type]): Map[String,Type] =
    (for v <- vars.toSet.flatMap(_.vars) yield v -> freshVar).toMap

  def replace(v:String,t:Type) =
    def repl(t2:Type) = Type.replace(t2)(using Map(v->t))
//    functions = functions.map(kv => kv._1 -> (kv._2._1.map(repl),repl(kv._2._2)))
//    connectors = for (k,(t1,t2,ctx)) <- connectors yield
//      k -> (t1.map(repl),t2.map(repl),ctx)
    ports = for (n,t) <- ports yield n -> repl(t)
    typeConstr = for (t1,t2) <- typeConstr yield (repl(t1),repl(t2))

  def getFun(f:String) =
    functions.getOrElse(f,sys.error(s"Unknown function $f"))
  def getConn(c:String): ()=>(Types,Types,Types,MutTypeCtxt) =
    connectors.getOrElse(c,sys.error(s"Unknown connector $c"))
  def getPort(p:String) =
    ports.getOrElse(p,sys.error(s"Unknown port $p"))

  def freshVar =
    seed +=1
    VarType(s"v§${seed-1}")

//  def copyScope: MutTypeCtxt =
//    new MutTypeCtxt(functions,connectors,ports=Map(),typeConstr=Nil,seed=0)
  def copyFuns: MutTypeCtxt =
    new MutTypeCtxt(functions,Map(),Map(),Nil,seed=0)

  def addInvFuns: Iterable[(String,(Types,Type))] =
    val overr = functions.keys.toSet intersect Prelude.invFunction.keys.toSet
    val ret = for f <- overr yield f -> functions(f)
    functions = functions ++ Prelude.invFunction
    ret

  def addFuns(more: Iterable[(String,(Types,Type))]) =
    functions = functions ++ more

  override def toString: String =
    //s"fun: ${functions.map(x=>s"${x._1}:${x._2._1.mkString(",")}->${x._2._2}").mkString("; ")}\n"+
    //s"con: ${connectors.map(x=>s"\n - ${x._1}:${x._2._1.mkString(",")}->${x._2._2.mkString(",")}\n>> ${x._2._3} <<").mkString}\n"+
    s"funs: ${functions.keys.mkString(",")}\n"+
      s"cons: ${connectors.keys.mkString(",")}\n"+
      s"ports: ${ports.map(x=>s"${x._1}->${x._2}").mkString("; ")}\n"+
      s"const: ${typeConstr.map(x=>s"${x._1}=${x._2}").mkString(", ")}\nseed: $seed"



object MutTypeCtxt:
  type Types = List[Type]
  type TypeConstraints = List[(Type,Type)]


  ////////////////
  // experimenting
  ////////////////

  val myCtx = new MutTypeCtxt(functions = Prelude.functions)





