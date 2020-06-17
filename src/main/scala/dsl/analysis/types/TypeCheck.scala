package dsl.analysis.types

import dsl.analysis.syntax.Statement
import dsl.backend.{In, Out, Show}
import dsl.common.{TypeException, UndefinedNameException}

/**
  * Created by guillecledou on 2020-01-07
  */


object TypeCheck {

  def solve(cons:Set[TCons],ctx:Context):Substitution = {
    // try to unify constraints (unsolvedCons have destructors that need another round)
    val (solvedCons,unsolvedCons) = Unify(cons)
    //println(s"Initial constraints: ${cons.mkString(",")}")
    var subst:Map[TVar,TExp] = Substitute(solvedCons)
    var substitute = Substitution(subst)
    //println(s"Substitution: ${subst.mkString(",")}")
    // try to substitute known variables in unsolved destructor constraints
    val substDestr:Set[TCons] = unsolvedCons.map(tc => TCons(substitute(tc.l),substitute(tc.r)))
//    println(s"Substituted destructors: ${substDestr.mkString(",")}")
    // expand destructors in unsolved constraints
    val expandDestrCons:Set[TCons]= substDestr.map(tc=> TCons(Destructor.expand(tc.l,ctx),Destructor.expand(tc.r,ctx)))
    //println(s"Expanded destructors: ${expandDestrCons.mkString(",")}")
    // try to unify expanded unsolved constraints
    val (solved,unsolved) = Unify.withReplacedDestr(expandDestrCons)
    //println(s"New solved from destructors: ${solved.mkString("\n")}")
    if (unsolved.nonEmpty)
      throw new TypeException(s"Impossible to unify type constraints:\n ${unsolved.map(Show(_)).mkString(",")}")
    // otherwise add new know variables to the substitution
    //println(s"Solved from before: ${subst.mkString("\n")}")
    subst=  Substitute(solved++subst)
    //println(s"Joint Solved : ${subst.mkString("\n")}")
    // return substitution
    Substitution(subst)
  }

  def numParams(actual:Int,formal:Int):Unit =
    if (actual!=formal) throw new TypeException(s"Expected ${formal} inputs, but ${actual} found")

  def isFunType(expr: TExp):TFun = expr match {
    case t@TFun(ins,outs) => t
    case e => throw new TypeException(s"Function type expected but ${e.getClass} found")
  }

  def isInterfaceType(expr:TExp):TExp = expr match {
    case TFun(_,_) => throw new TypeException(s"Interface type expected but Function type found")
    case _ => expr
  }

  def lhsAssigAreVars(vars:List[String],ctx:Context):Unit = {
    var err: Option[ContextEntry] = None
    vars.exists(v =>
      if (ctx.context.contains(v) && !ctx.ports.contains(v)) {err = Some(ctx.context(v)); true} else false)
    if (err.isDefined)
      throw new TypeException(s"Only variables can be used on the LHS of an assignment but ${err.getClass} found")
    if (vars.toSet.size != vars.size)
      throw new TypeException(s"Cannot repeat variables on the LHS of an assignment but ${vars.mkString(",")} found")
  }

  def isDestr(t:TExp):Boolean = t match {
    case TDestr(t1) => true
    case _ => false
  }

  def wellDefinedType(te:TExp,tDef: TExp,ctx:Context):Boolean =
    existsType(te,ctx) && matchType(te,tDef)

  private def existsType(te:TExp,ctx:Context):Boolean = te match {
    case TBase(name,ps) if ctx.adts.contains(name) && ps.forall(p=>existsType(p,ctx)) => true
    case TBase(name,ps) => throw new UndefinedNameException(s"Unknown type name ${name}")
    case TTensor(t1,t2) => existsType(t1,ctx) && existsType(t2,ctx)
    case TFun(ins,outs) => existsType(ins,ctx) && existsType(outs,ctx)
    case _ => true
  }
  private def matchType(te:TExp,tdef:TExp):Boolean = try {
    Unify(Set(TCons(te,tdef)))
    true //horrible
  } catch {
    case e:TypeException => throw new TypeException(s"Not well defined type ${Show(te)}")
  }

  def isClosed(s:Statement,ports:Map[String,List[PortEntry]]):Unit = {
    //ports.forall(p=> isClosedPort(p._2))
    var err: Option[String] = None
    ports.forall(p=> if (isClosedPort(p._2)) true else {err=Some(p._1);false})
    if (err.isDefined)
      throw new TypeException(s"Port ${err.get} is not closed in:\n ${Show(s)}")
  }
  private def isClosedPort(occurrences:List[PortEntry]):Boolean = {
    val in = occurrences.find(p => p.pType == In)
    val out = occurrences.find(p=> p.pType == Out)
    in.isDefined && out.isDefined
  }




}
