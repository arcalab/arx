package dsl.revised.typing

import dsl.revised
import revised.core.{Automaton, Connector, Network, Rule, Show, Term}
import revised.typing.Type._
import Network.{Constructor, Link}
import Connector.{CAut, CNet}
import Term.{Fun, IntVal, Var}
import dsl.revised.Error.Who
import dsl.revised.Prelude.{boolType, intType}
import revised.{Error, Prelude}

object Infer:
  /** Infer a valid typing context for a connector, and throws an exception if it fails */
  def apply(c:Connector): MutTypeCtxt =
    implicit val ctx = new MutTypeCtxt(functions = Prelude.functions)
    inferType(c)
    unify
    ctx
  /** Infer a typing context for a network, and throws an exception if it fails */
  def apply(net:Network): MutTypeCtxt =
    apply(CNet(net,Nil,Nil,Nil))

  // temporary
  def justCollect(n:Network): MutTypeCtxt = justCollect(CNet(n,Nil,Nil,Nil))
  def justCollect(c:Connector): MutTypeCtxt =
    implicit val ctx = new MutTypeCtxt(functions = Prelude.functions)
    inferType(c)
    //unify
    ctx
  /** Type the flattened automata of a network, using the typing information of the network. */
  def typeAut(net:Network): (Automaton,MutTypeCtxt) =
    implicit val ctx = new MutTypeCtxt(functions = Prelude.functions)
    inferType(CNet(net,Nil,Nil,Nil))
    //unify
    val a = net.toAut
    inferType(a)
    unify
    (a,ctx)

  /** Collects the typing context of a connector, without checking type constraints,
    *  and throws an exception if it fails. */
  def inferType(c:Connector)(using ctx:MutTypeCtxt): Unit = c match
    case CNet(net,_,_,_) => inferType(net)
    case CAut(aut,_,_,_) => inferType(aut)

  //////////////
  // AUTOMATA //
  //////////////

  /** Infer the type of an automaton */
  def inferType(a:Automaton)(using ctx:MutTypeCtxt): Unit = a match
    case Automaton(init,inv,rs,ins,outs,reg,clocks,_) =>
      init.foreach(inferType)
      rs.foreach( r => {
        r.pred.foreach(inferBoolType)
        r.eqs.foreach(inferType)
        r.upd.foreach(inferType)
      })
      // no need to add ins, outs, regs to known ports, since no type restrictions are added.
      clocks.foreach(c => ctx.addPort(c,intType))
      val overr = ctx.addInvFuns
      inv.foreach(inferBoolType)
      ctx.addFuns(overr)


  //      if init.nonEmpty then
//        inferType(init.head)
//        inferType(Automaton(init.tail,inv,rs,ins,outs,reg)) // continue
//      else if rs.nonEmpty then
//        val r = rs.head
//        r.pred.foreach(inferType)
//        r.assg.foreach(inferType)
//        r.upd .foreach(inferType)
//        inferType(Automaton(init,inv,rs-r,ins,outs,reg)) // continue
//      else
//        inv.foreach(inferType)

  /** Infers the type of an assignment */
  def inferType(assg:Rule.Assignment)(using ctx:MutTypeCtxt): Unit =
    val typ = inferType(assg.t)
    ctx.addPort(assg.v,typ)

  /** Infers the type of a term, updating the context */
  def inferType(t: Term)(using ctx:MutTypeCtxt): Type = t match
    case Var(v) => ctx.newPort(v)
    case IntVal(_) => Prelude.intType
    case Fun(name, terms) if ctx.functions contains name =>
      val (inTs,outT) = ctx.functions(name)
      if inTs.size != terms.size then
        Error.typing(s"function $name has ${terms.size} arguments, but expected ${inTs.size}.")
      // type all terms
      val termTypes = for t<-terms yield inferType(t)
      // use fresh variables for the function types
      implicit val subst = ctx.freshSubst(outT::inTs)
//      val (inTs2,outTs2,_) = ctx.freshen(inTs,List(outT),Nil)
      ctx.addTCons(replace(inTs) zip termTypes)
      replace(outT)
    case _ =>
      Error.typing(s"unknown function $t (functions: ${ctx.functions.keys.mkString(",")})")

  def inferBoolType(t:Term)(using ctx:MutTypeCtxt): Unit =
    val typ = inferType(t)
    if typ!=boolType then Error.typing(s"Should be boolean: ${Show(t)}: $typ")

  /////////////
  // NETWORK //
  /////////////

  /** Infers the type of a network */
  def inferType(net: Network)(using ctx:MutTypeCtxt): Unit =
    // Data
    for (name,(args,constr)) <- net.data; Constructor(c,as) <- constr do
      val dataType = BaseType(name,args.map(VarType.apply))
      ctx.functions ++= Seq(
        c -> (as -> dataType),
        s"is§$c" -> (List(VarType("a")) -> boolType)
        //s"get§$c" -> (List(intType,VarType("a")) -> ...)
      )
      for (a,i)<-as.zipWithIndex do
        ctx.functions += s"get§$c§$i" -> (List(dataType) -> a)
    // Connectors
    for (name,conn) <- net.connectors do
      val ctx2 = ctx.copyFuns
      def inferLater(): (List[String],List[String],List[String],MutTypeCtxt) =
        Error.debug(s"=== inferring type for $name")(using Who("Infer"))
        inferType(conn)(using ctx2)
        //println(s"[infering later] $name - $conn - $ctx2")
        (conn.args,conn.ins,conn.outs,ctx2)
      ctx.addConn(name,inferLater)
    // Links (invocations)
    for Link(n,args,ins,outs) <- net.links do
      if n=="" then // Link without a name is a Sync/ID connection - type it accordingly
        val insT =  for (i<-ins)  yield ctx.newPort(i)
        val outsT = for (o<-outs) yield ctx.newPort(o)
        ctx.typeConstr = ctx.typeConstr :::(insT zip outsT)
      else
        val (argTs,inTs,outTs,ctx2) = ctx.getConn(n)()
        if args.size != argTs.size then
          Error.typing(s"connector $n applied with wrong number of arguments - '${args.mkString(",")}' with expected type '${argTs.mkString(",")}'")
        if ins.size != inTs.size then
          Error.typing(s"connector $n applied with wrong number of inputs - '${ins.mkString(",")}' with expected type '${inTs.mkString(",")}'")
        if outs.size != outTs.size then
          Error.typing(s"connector $n returns wrong number of outputs - '${outs.mkString(",")}' with expected type '${outTs.mkString(",")}'")
        // add input and output types based on connector type
        val lr: (List[Type],List[Type]) = ctx2.typeConstr.unzip // types infered by ctx2
//        println(s"[Infer] got type of $n. TConstr: $lr")
        val argTs1 = args.map(inferType)
//        println(s"[Infer] Replacing vars: ${argTs:::inTs:::outTs:::lr._1:::lr._2}")
//        println(s"[Infer] ctx before: ${ctx}")
        implicit val subst: Map[String,Type] = ctx.freshSubst(argTs:::inTs:::outTs:::lr._1:::lr._2) // fixed: argTs instead of argTs1
//        println(s"[Infer] got subst ${subst}. new ctx: ${ctx}")
  //      val (inTs2,outTs2,tc2) = ctx.freshen(inTs,outTs,ctx2.typeConstr)
        for (i,t) <- ins.zip(replace(inTs)) do ctx.addPort(i,t)
        for (o,t) <- outs.zip(replace(outTs)) do ctx.addPort(o,t)
        ctx.typeConstr = ctx.typeConstr ::: (replace(lr._1) zip replace(lr._2)) ::: (argTs1 zip replace(argTs))
//        println(s"[Infer] ctx after: ${ctx}")


  /////////////////
  // Unification //
  /////////////////

  /** Unifies the type constraints in a typing context,
    * by replacing variables that match in all context. */
  def unify(using ctx: MutTypeCtxt): Unit = ctx.typeConstr match
    case Nil =>
    case (t1,t2)::rest if t1==t2 =>
      ctx.typeConstr = rest
      unify
    case (VarType(v),t)::rest =>
      ctx.typeConstr = rest
      ctx.replace(v,t)
      unify
    case (t,VarType(v))::rest =>
      ctx.typeConstr = (VarType(v),t)::rest
      unify
    case (BaseType(n1,args1),BaseType(n2,args2))::rest
      if n1 == n2 && args1.size == args2.size =>
      ctx.typeConstr = args1.zip(args2):::rest
      unify
    case _ =>
      Error.typing(s"types do not unify - ${
        ctx.typeConstr.head._1} and ${ctx.typeConstr.head._2}.") // Current context:\n$ctx")

