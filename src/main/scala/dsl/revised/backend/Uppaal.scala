package dsl.revised.backend

import dsl.revised.Prelude
import dsl.revised.core.{Automaton, Network, Term}
import dsl.revised.core.Rule
import dsl.revised.core.Rule.Assignment
import dsl.revised.core.Term.{Fun, Var}
import dsl.revised.typing.{Infer, MutTypeCtxt, Type}
import dsl.revised.Error.{UppaalEncodingError, uppEncoding}

object Uppaal :
  def autToUppaal: String =
    """<?xml version="1.0" encoding="utf-8"?>
      |<!DOCTYPE nta PUBLIC '-//Uppaal Team//DTD Flat System 1.1//EN' 'http://www.it.uu.se/research/group/darts/uppaal/flat-1_2.dtd'>
      |<nta>
      |	<declaration>
      |  XXX
      | </declaration>
      |	<template>
      |		<name>XXX</name>
      |		<declaration>XXX</declaration>
      |		<location id="idXXX" x="535" y="17">
      |			<name x="493" y="34">LabelXXX</name>
      |		</location>
      |  	<init ref="idXXX"/>
      |		<transition>
      |			<source ref="idXXX"/>
      |			<target ref="idXXX"/>
      |			<label kind="guard" x="501" y="-34"> XXX </label>
      |			<label kind="synchronisation" x="833" y="93"> XXX </label>
      |			<label kind="assignment" x="833" y="110"> XXX </label>
      |  </transition>
      | </template>
      | <system> XXX
      | </system>
      |	<queries>
      |		<query>
      |			<formula>XXX</formula>
      |			<comment>XXX</comment>
      |		</query>
      |  </queries>
      |</nta>""".stripMargin

  private case class Edge(sel:String="",guard:String="",sync:String="",upd:String=""):
    override def toString: String =
      (if sel.nonEmpty then s"\n  sel: $sel" else "") ++
      (if guard.nonEmpty then s"\n  guard: $guard" else "") ++
      (if sync.nonEmpty then s"\n  sync: $sync" else "") ++
      (if upd.nonEmpty then s"\n  upd: $upd" else "")

  def netToUppaal(net:Network): String =
    val (aut,typeCtx) = Infer.typeAut(net)
    val (init,usedTypes) = getInit(aut,typeCtx)
    val (rules,usedTypes2) = getRules(aut,typeCtx)
    val (inv,usedTypes3) = getInv(aut,typeCtx)
    val dataDecl = getData(usedTypes++usedTypes2++usedTypes3,net.data)
//    val invs = getInv(aut,typeCtx)
    s">>> data: $dataDecl\n>>> init: $init\n>>> inv $inv\n>>> rules: ${rules.mkString("\n---\n")}"

  def getData(usedTypes: Set[String], dataDecl: Map[String,(List[String],List[Network.Constructor])]): String =
    val x = for uTyp <- usedTypes yield
      uTyp match
        case "Int" => ""// s"typedef int[2] Int;" //
        case "Bool" => ""//s"typedef bool[2] Bool;'"//s"typedef int[-1,1] Bool;" // und | False | True
        case "Unit" => "typedef int[0,0] Unit; const Unit UnitC = 0;" // und | ()
        case _ =>
          if !dataDecl.contains(uTyp) || dataDecl(uTyp)._1.nonEmpty || dataDecl(uTyp)._2.exists(_.args.nonEmpty)
          then uppEncoding(s"Cannot use data type '${uTyp} - complex or missing data type")
          else
            // uTyp = constr1 | constr2 | ... -> typedef int[0,{constr.size-1}] uTyp; const uTyp constr(0) - 0; ...
            val constrs = dataDecl(uTyp)._2.map(_.name).zipWithIndex.map((s,i)=>s"const $uTyp $s = $i;")
//            s"typedef int[0,${constrs.size}] ${uTyp}_;\ntypedef ${uTyp}_[2] ${uTyp}\n${constrs.mkString("\n")}"
            s"typedef int[0,${constrs.size}] ${uTyp};\n${constrs.mkString("\n")}"
    x.mkString("\n")


  private def getInv(a:Automaton,types:MutTypeCtxt): (String,Set[String]) =
    val (x,y) = (for term <- a.inv yield getTerm(term,types)).unzip
    (x.mkString(" && "),y.flatten)

  // generates the declaration and initialisation of variables, and also returns
  // the names of the data types that were used.
  def getInit(a:Automaton,types:MutTypeCtxt): (String,Set[String]) =
    val x = for (asgm <- a.init) yield
      getInitAssgm(asgm,types)
    val (ini,types2) = x.unzip
    val regs = a.registers -- a.init.map(_.v) -- a.clocks
    val regs2 = for m <- regs yield getInit(m,types)
    val clocks = a.clocks.map(c=>s"clock ${fixVar(c)};").mkString("\n")
    ((regs2++ini+clocks).mkString("\n"),types2.flatten)

  private def getInitAssgm(a:Assignment,types:MutTypeCtxt): (String,Set[String]) =
    if !isConcreteTerm(a.t) then
      uppEncoding(s"Cannot assign a term with variables: ${a.v} := ${a.t}")
    val t = types.getConcreteType(a.v)
    val (e,ts2) = getTerm(a.t,types)
    (s"bool ${fixVar(a.v)}_set = true; ${fixType(t)} ${fixVar(a.v)} = $e;", ts2 + t.toString)

  private def getAssgm(a:Assignment,clocks:Set[String],types:MutTypeCtxt): (String,Set[String]) =
    val set = if clocks(a.v) then "" else s"${fixVar(a.v)}_set = true; "
    val (e,ts2) = getTerm(a.t,types)
    (s"$set${fixVar(a.v)} = ${getTerm(a.t,types)._1};" , ts2)

  private def isConcreteTerm(term: Term): Boolean = term match {
    case Var(_) => false
    case Fun(_, l) => l.forall(isConcreteTerm)
    case Term.IntVal(_) => true
  }

  private def getInit(v:String,types:MutTypeCtxt): String =
    val t = types.getConcreteType(v)
    s"${fixType(t)} ${fixVar(v)}; bool ${fixVar(v)}_set = false;"

  private def getTerm(t:Term, types:MutTypeCtxt): (String,Set[String]) = t match
    case Var(name) =>  (fixVar(name),Set(types.getConcreteType(name).toString))
    case Fun("()",Nil) => ("0",Set(Prelude.unitType.name))
    case Fun("True",Nil) => ("true",Set(Prelude.boolType.name))
    case Fun("False",Nil) => ("false",Set(Prelude.boolType.name))
    case Fun(name,Nil) => (name,Set(types.functions(name)._2.toString))
    case Fun(name, t1::t2::Nil) if name.forall("+-><!%/*=|&".toSet)=>
      val myType = types.functions(name)._2.toString
      val (t1_,e1) = getTermMP(t1,types)
      val (t2_,e2) = getTermMP(t2,types)
      (s"$t1_ $name $t2_", e1 ++ e2 + myType)
    case Fun(name, terms) =>
      val t2 = types.functions(name)._2.toString
      val e2 = terms.flatMap(x => getTerm(x,types)._2).toSet
      (t.toString, e2 + t2)
    case Term.IntVal(i) => (i.toString, Set(Prelude.intType.name))

  private def getTermMP(t:Term,ts:MutTypeCtxt) =
    val (t2,ts2) = getTerm(t,ts)
    t match
      case Fun(_,_::_::Nil) => (s"($t2)",ts2)
      case _ => (t2,ts2)


  private def getRules(aut: Automaton, ctxt: MutTypeCtxt): (Set[Edge],Set[String]) =
    val (x,usedTs) = (for r <- aut.rs yield getRule(r,aut,ctxt)).unzip
    (x,usedTs.flatten)

  private def getRule(r:Rule, a:Automaton, ctxt:MutTypeCtxt): (Edge,Set[String]) =
    val allRead = r.get++r.ask
    val (regRead,portsRead) = allRead.partition(a.registers)
    //val regWritten = r.upd.map(_.v)
    val portsWritten = r.eqs.map(_.v)
    val (predTerms,usedTypes1) = r.pred.map(getTerm(_,ctxt)).unzip
    val (eqsTerms,usedTypes2) = r.eqs.map(x=>(x.v -> getTerm(x.t,ctxt))).map(x=>((x._1,x._2._1),x._2._2)).unzip
    val (updAssg,usedTypes3) = r.upd.map(getAssgm(_,a.clocks,ctxt)).unzip
    val guards = regRead.filter(!a.clocks(_)).map(x=>s"${fixVar(x)}_set") ++ predTerms.map("("+_+")")
    /// FIX
    val upd = updAssg ++ r.get.map(x=>s"${fixVar(x)}_set = false;")
    val sync = portsRead.zipWithIndex.map((x,i)=>s"$x[in$i]?")++eqsTerms.map(x=>s"${x._1}[${x._2}]!")
    val allUsedTypes = (usedTypes1++usedTypes2++usedTypes3).flatten
    ( Edge(guard=guards.mkString(" && "), sync=sync.mkString(", "), upd=upd.mkString(" "))
      , allUsedTypes)


  private def fixVar(str: String) = str.replaceAll("§","_")
  private def fixType(str: Type) = str.toString match
    case "Int" => "int"
    case "Bool" => "bool"
    case x => x



