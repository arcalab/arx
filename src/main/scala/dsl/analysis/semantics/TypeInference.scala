package dsl.analysis.semantics

import dsl.analysis.syntax._
import dsl.common.{TypeException, UndefinedVarException}
import dsl.DSL
import preo.ast.CPrim
import preo.backend.Network
import preo.backend.Network.Prim


/**
  * Created by guillecledou on 2019-06-03
  */

object TypeInference {

  private var tVars:Int = 0
  private def freshVar():String = {tVars+=1; (tVars-1).toString}

//  class Context {
//    protected val ctx: Map[String,TVar] = Map()
//
//    def get:Map[String,TVar] = ctx
//
//    def contains(x:String):Boolean = ctx contains x
//
//    def apply(x:String):TVar = ctx(x)
//
//    // todo: handle multiple definitions of a variable, for now assume it doesn't happen
//    def add(x:String,t:TVar):Context = {
//      var oldCtx = ctx
//      new Context {
//          override val ctx = oldCtx + (x -> t)
//        }
//    }
//    // todo: handle multiple definitions of a variable, for now we don't have different scopes
//    def join(other:Context):Context = {
//      var oldCtx = ctx
//      var newCon:Context = other
//      for ((k,v) <- oldCtx) {
//        newCon = newCon.add(k,v)
//      }
//      newCon
//    }
//  }

  /* todo: how to handle cases like:
      -----Case1-----
      x = Nil ---> is going to say that x should be of type List<T>
      y = Cons(Zero,x) ---> is going to say that x should be of type List<Nat>
      z = Cons(True,x) ---> is going to say that x should be of type List<Bool>
      PROBLEM: the unification is going to fail
      -----Case2-----
      x = Nil ---> is going to say that x should be of type List<T>
      PROBLEM?: if x is never use on the rhs, T is never going to be match with a concrete type
  */
  /**
    * Infers the type of the each assignment/expression of the AST by means of constraint typing
    * @param ast
    * @param adt
    * @return a constraint typing relation: (context, type of the last expression, set of type constraints)
    */
  def infer(ast:AST):(Context,Map[String,TypeConn],TypeExpr,Set[TCons]) = {
    // find known adt from the ast
    var adt:Map[Variant,TypeDecl] = ast.getTypes.flatMap(td => td.variants.map(v => v -> td)).toMap
    // initialize type variables
    tVars = 0
    // initialize context
    var ctx = new Context
    // initialize constraints
    var tCons:Set[TCons] = Set()
    // get all connector definitions
    var conns = ast.getDefs
    /*
    * todo:
    *  eventually we will have to pass to the next definition the previous context
    *  this is when we allow conn defs to reference connectors from other previous defs.
    */
    // for each connector def infer its type
//    var tConns:Map[String,(Context,TypeExpr,Set[TCons])] = conns.map(c=> c.name->infer(ctx,c,adt)).toMap
    var tConns:Map[String,TypeConn] = conns.map(c => c.name -> TypeConn(c)).toMap
    // for each conn create a TVar
//    var connVars:Map[String,TVar] = tConns.map(tc => tc._1->TVar(freshVar()))
    // for each conn add a constraint from its TVar to its actual type
//    tCons = connVars.map(cv => TCons(cv._2,tConns(cv._1)._2)).toSet
    // add to the context the name of the connector and its type
//    ctx = connVars.map(cv => ctx.add(cv._1,cv._2)).foldRight(ctx)(_.join(_))
    // get all assignments (for now are the only expressions to type)
    var assig = ast.getAssignments
    // for each assignment infer its type, using the contex from previous inferred assignments
    // accumulate all the constraints from each inferred assignment
    var res:(Context,TypeExpr,Set[TCons]) = (new Context,TUnit,Set[TCons]())
    for (a <- assig) {
      res = infer(ctx,tConns,a,adt)
      ctx = res._1
      tCons ++= res._3
    }
    // return the last known context, the type of the last assignment and the accumulated set of constraints
    (res._1,tConns,res._2,tCons)
  }
//  def infer(ast:AST):(Context,TypeExpr,Set[TCons]) = {
//    // find known adt from the ast
//    var adt:Map[Variant,TypeDecl] = ast.getTypes.flatMap(td => td.variants.map(v => v -> td)).toMap
//    // initialize type variables
//    tVars = 0
//    // initialize context
//    var ctx = new Context
//    // initialize constraints
//    var tCons:Set[TCons] = Set()
//    // get all connector definitions
//    var conns = ast.getDefs
//    /*
//    * todo:
//    *  eventually we will have to pass to the next definition the previous context
//    *  this is when we allow conn defs to reference connectors from other previous defs.
//    */
//    // for each connector def infer its type
//    var tConns:Map[String,(Context,TypeExpr,Set[TCons])] = conns.map(c=> c.name->infer(ctx,c,adt)).toMap
//    // for each conn create a TVar
//    var connVars:Map[String,TVar] = tConns.map(tc => tc._1->TVar(freshVar()))
//    // for each conn add a constraint from its TVar to its actual type
//    tCons = connVars.map(cv => TCons(cv._2,tConns(cv._1)._2)).toSet
//    // add to the context the name of the connector and its type
//    ctx = connVars.map(cv => ctx.add(cv._1,cv._2)).foldRight(ctx)(_.join(_))
//    // get all assignments (for now are the only expressions to type)
//    var assig = ast.getAssignments
//    // for each assignment infer its type, using the contex from previous inferred assignments
//    // accumulate all the constraints from each inferred assignment
//    var res:(Context,TypeExpr,Set[TCons]) = (new Context,TUnit,Set[TCons]())
//    for (a <- assig) {
//      res = infer(ctx,a,adt)
//      ctx = res._1
//      tCons ++= res._3
//    }
//    // return the last known context, the type of the last assignment and the accumulated set of constraints
//    (res._1,res._2,tCons)
//  }

  /**
    * Given an assignment id = expression, finds the type of the id and the expr
    * by means of constraint typing
    *
    * @param ctx
    * @param asg
    * @param adt
    * @return a new constraint typing relation: (context, type of the id, set of type constraints)
    */
  private def infer(ctx:Context,ctxConn:Map[String,TypeConn],asg:Assignment, adt:Map[Variant,TypeDecl]): (Context,TypeExpr,Set[TCons]) = {
    // asg: id = expr
    val id:Identifier = asg.variable
    val expr:Expr = asg.expr

    // create a new Type Variable for id
    val T = TVar(freshVar())
    // add to the context that id is of type T
    var newCtx = ctx.add(id.name,T)
    // infer the type of the expression, with the new context
    val (ctxexp,texp,cexp) = infer(newCtx,ctxConn,expr,adt)
    // create a new constraint for T = texp
    val newConst = TCons(T,texp)
    // return:
    // - the new context return by the exprs (already contains newCtx)
    // - the type of the assignment, for now is the type of the id
    // - the set of constraints with the addition of Tid = Texp
    (ctxexp,T,cexp+newConst)
  }

  /**
    * Given a expression, finds the type of the expression based on the kind of expression
    * (adtTerm, adtConst, or id for now) by means of constraint typing
    *
    * @param ctx
    * @param expr
    * @param adt
    * @return a new constraint typing relation: (context, type of the expression, set of type constraints)
    */
  private def infer(ctx:Context, ctxConn:Map[String,TypeConn], expr:Expr, adt:Map[Variant,TypeDecl]):(Context,TypeExpr,Set[TCons]) = expr match {
    case Identifier(n) =>
      // if id is defined already return the type of id, other wise it is undefined
      if (ctx.contains(n))
        (ctx,ctx(n),Set())
      else throw new UndefinedVarException("Unknown identifier: " + n)
    case t@AdtTerm(n) => try {
      // find the type of the adt term
      var variant:Variant = adt.find(k => k._1.name == n).get._1
      var ttype = getVariantType(adt(variant).name)
      // replace all parametric types with new variables accordingly
      ttype = mkNewParametricTVar(ttype,Map())._1
      // create a new variable
      var fresh = TVar(freshVar())
      // add the corresponding constraint
      var tcons = TCons(fresh,ttype)
      //todo: see how to handle TVar in this case
      (ctx, fresh, Set(tcons))
      } catch {
        case e:NoSuchElementException => throw new TypeException("Unknown variant name: " + n)
      }
    case c@AdtConsExpr(n, ps) => try {
      // find the type of the constructor
      var variant = adt.find(k => k._1.name == n).get._1
      var rctype = getVariantType(adt(variant).name)
      // find the type of the parameters
      var ctype = getFormalParamsTypes(variant,adt)
      // replace all parametric types with new variables accordingly in the parameters and in the type of the constructor
      val (cctype,m) = mkNewParametricTVars(ctype,Map())//._1
      rctype = mkNewParametricTVar(rctype,m)._1
      // infer the types of the actual parameters
      var pstypes = ps.map(p => infer(ctx,ctxConn,p,adt))
      // mk constructor constraints from each actual param to each formal param
      // it was already checked at parsing if the number of params matched
      var paramsConst:Set[TCons] = cctype.zip(pstypes.map(pt => pt._2)).map(p => TCons(p._1,p._2)).toSet
      // mk the type of this expression:
      var T = TVar(freshVar())
      // mk new constrait for T and for the fresh variable
      var newCons = TCons(T,rctype)
      // mk new Context from inferred context
      var newCtx:Context = pstypes.map(pt => pt._1).foldRight(ctx)(_.join(_))
      (newCtx, T, paramsConst++pstypes.flatMap(pt=> pt._3)+(newCons))
      } catch {
        case e:NoSuchElementException => throw new TypeException("Unknown variant name: " + n)
      }
    case ConnId(n,ps) =>
      // find the type of the connector with name n
      var tconn:TypeConn = ctxConn(n)
      // find the type of each actual parameter of the connector
      // todo: if it has no parameters, assume no concrete data is sent ~~ unit for now
      var psTypes = ps.map(p => infer(ctx,ctxConn,p,adt))
      // create new variables for each abstract var type in the formal parameters (inputs)
      var renamInsTypes= mkNewParametricTVars(tconn.ins,Map())
      var renamOutsTypes = mkNewParametricTVars(tconn.outs,renamInsTypes._2)
      // add a constraint from each formal param to each actual param
      var paramConst:Set[TCons] = renamInsTypes._1.zip(psTypes.map(pt=>pt._2)).map(p => TCons(p._1,p._2)).toSet
      // mk a fresh var for the type of this expression
      var T = TVar(freshVar())
      // mk a new constraint saying that the type of this expression is of type T
//      var connInsType:TypeExpr =
//        if (tconn.ins.size>1) tconn.ins.init.foldRight[TypeExpr](tconn.ins.last)(TMap(_,_))
//        else  if (tconn.ins.isEmpty) TUnit else tconn.ins.head
//      var connOutsType:TypeExpr = if (tconn.ins.size>1) TProd(tconn.ins.head,tconn.ins.tail)
//      else  if (tconn.ins.isEmpty) TUnit else tconn.ins.head
      var connType = TypeConn(renamInsTypes._1,renamOutsTypes._1).getType//tconn.getType
      var newCons = paramConst++Set(TCons(T,connType))++psTypes.flatMap(pt=>pt._3)
      (ctx,T,newCons)
//      (ctx,TUnit,Set())
  }

//  private def infer(ctx:Context,ctxConn:Map[String,TypeExpr],connDef:ConnDef,adt:Map[Variant,TypeDecl]):(Context,TypeExpr,Set[TCons]) = {
//    def mkTCons(tvars:List[TypeExpr]):Set[TCons] = tvars match {
//      case Nil => Set()
//      case h::Nil=> Set()
//      case h::tail => mkTCons(tail)+TCons(h,tail.head)
//    }
//    // convert the defined connector into a network
//    var network = Network(DSL.unsafeCoreConnector(connDef.c))
//    // check if the network is closed or doesn't have outputs
//    (network.ins,network.outs) match {
//      // closed network
//      case (Nil,Nil) => (ctx,TUnit,Set())
//      // no outputs
//      case (ins,Nil) =>
//        // for each in create a new freshvar
//        var Tins = ins.map(i=>TVar(freshVar()))
//        // the type of the connector is Tin1 -> ... -> TinN -> TUnit
//        var TConn = Tins.foldRight[TypeExpr](TUnit)(TMap(_,_))
//        // make fresh var to be the type of the connector and add the corresponding constraint
//        var T = TVar(freshVar())
//        (ctx,TConn,Set())
//      // possible inputs and definitely outputs
//      case (ins,outs) =>
//        // infer the type for each prim
//        var Tprims = network.prims.flatMap(p=> infer(p))
//        // group types by port
//        var portTypes = Tprims.groupBy(_._1).mapValues(_.map(_._2))
//        // make for each port that has more that one type variable, make a constraint for them to be equal
//        var newCons = portTypes.flatMap(pt=> mkTCons(pt._2)).toSet
//        // make the type of the connector: Tin1 -> ... -> TinN -> Tout1 x ... x ToutN
//        var TConn = (ins++outs.init).map(p=> portTypes(p).head)
//          .foldRight[TypeExpr](portTypes(outs.last).head)(TMap(_,_))
//        (ctx,TConn,newCons)
//    }
//  }

//  /**
//    * Get the type of a primitive connector
//    *
//    * @param prim
//    * @return a map from each port to its type
//    */
//  private def infer(prim:Prim):Map[Int,TypeExpr] = prim match {
//    case Prim(CPrim(name,_,_,_),List(a),List(b),_) if name.matches("(sync|fifo|id)")=>
//      val T = TVar(freshVar())
//      Map(a->T,b->T)
//    case Prim(CPrim("lossy",_,_,_),List(a),List(b),_) =>
//      val T = TVar(freshVar())
//      Map(a->T,b->TOpt(T))
//    case Prim(CPrim("drain",_,_,_),List(a,b),List(),_) =>
//      Map(a->TVar(freshVar()),b->TVar(freshVar()))
//    case Prim(CPrim("fifofull",_,_,_),List(a),List(b),_) =>
//      // special case, todo: add support to declare the type of the fifofull when using it
//      // for now assume unspecified
//      val T = TVar(freshVar())
//      Map(a->T,b->T)
//    case Prim(CPrim("merger",_,_,_),List(a,b),List(c),_) =>
//      val Ta = TVar(freshVar())
//      val Tb = TVar(freshVar())
//      Map(a->Ta,b->Tb,c->TEithers(Ta,List(Tb)))
//    case Prim(CPrim("dupl",_,_,_),List(a),List(b,c),_) =>
//      val T = TVar(freshVar())
//      Map(a->T,b->T,c->T)
//    case Prim(CPrim("writer",_,_,_),List(),List(a),_) =>
//      // special case, todo: add support to declare what the writer writes
//      // for now assume unspecified
//      Map(a->TVar(freshVar()))
//    case Prim(CPrim("reader",_,_,_),List(a),List(),_) =>
//      Map(a->TVar(freshVar()))
//    // a node that is only a duplicator, i.e. has one input only
//    case Prim(CPrim("node",_,_,extra), List(a), outs, _) if extra.intersect(Set("dupl")).nonEmpty =>
//      val Ta = TVar(freshVar())
//      val Touts = outs.map(o=> o->Ta).toMap
//      Touts+(a->Ta)
//    // a node that is a merger and possible a duplicator, i.e. has more than one input
//    case Prim(CPrim("node",_,_,extra), ins, outs, _) if extra.intersect(Set("dup","mrg")).nonEmpty =>
//      val varsIns = ins.map(i => TVar(freshVar()))
//      val Tout = TEithers(varsIns.head, varsIns.tail)
//      ins.zip(varsIns).toMap++outs.map(o=> o->Tout).toMap
//    case Prim(CPrim("node",_,_,extra), ins, outs, _) if extra.intersect(Set("xor")).nonEmpty =>
//      val varsIns = ins.map(i => TVar(freshVar()))
//      val Tout = TOpt(TEithers(varsIns.head, varsIns.tail))
//      ins.zip(varsIns).toMap++outs.map(o=> o->Tout)
//    // any unknown prim 1->1
//    case Prim(CPrim(name,_,_,_),List(a),List(b),_) =>
//      val T = TVar(freshVar())
//      Map(a->T,b->T)
//    case Prim(p, ins,outs,_) =>
//      throw new TypeException(s"Unknown type for primitive ${p.name}")
//  }


  /**
    * Given a ADT variant, return the type of its formal parameters, if any
    *
    * @param v
    * @param adt
    * @return
    */
  def getFormalParamsTypes(v:Variant, adt:Map[Variant,TypeDecl]):List[TypeExpr] = v match {
    case AdtVal(n) => List()
    case AdtConst(n,ps) =>
      //find the expected type expression of each parameter
      var tp = ps.map(p => getVariantType(p))
      tp
  }

  /**
    * Given a TypeName return its type:
    * a) An abstract type name returns a type variable with the same name
    * b) A concrete type name returns a BaseType with the same name, and the corresponding type of each parameter
    * @param tn
    * @return
    */
  def getVariantType(tn:TypeName):TypeExpr = tn match {
    case AbsTypeName(atn) => TVar(atn)// TVar(freshVar())
    case ConTypeName(ctn,ps) => BaseType(ctn,ps.map(p => getVariantType(p)))
  }

  /**
    * Given a list of type expression (of parameters) which may have parametric types (abstract types)
    * creates a new parametric type variable for each abstract type, and replace each
    * appearance of the abstract type with the corresponding variable
    * E.g.: List<a> -> List<T> for some new T
    *
    * @param tes
    * @param map
    * @return
    */
  def mkNewParametricTVars(tes:List[TypeExpr],map:Map[TVar,TVar]):(List[TypeExpr],Map[TVar,TVar]) = {
    var nmap = map
    var ntes = List[TypeExpr]()
    for (te <- tes) {
      var res = mkNewParametricTVar(te,nmap)
      nmap = res._2
      ntes ++= List(res._1)
    }
    (ntes,nmap)
  }
  /**
    * Given a type expression which may have parametric types
    * creates a new type variable for each abstract type, and replace each
    * appearance of the abstract type with the corresponding variable.
    * E.g.: List<a> -> List<T> for some new T
    *
    * @param tes
    * @param map
    * @return
    */
  def mkNewParametricTVar(te: TypeExpr,map:Map[TVar,TVar]):(TypeExpr,Map[TVar,TVar]) = te match {
    case TUnit => (TUnit,map)
    case t@TVar(n) if n.matches("[a-z][a-zA-Z0-9_]*") =>
      if (map.contains(t))
        (map(t),map)
      else {
        var fresh = TVar(freshVar())
        (fresh,map+(t->fresh))}
    case TMap(from, to) =>
      val (nfrom,nmapf) = mkNewParametricTVar(from,map)
      val (nto,nmapt) = mkNewParametricTVar(to,nmapf)
      (TMap(nfrom,nto),nmapt)
    case BaseType(n,ps) =>
      var nres = mkNewParametricTVars(ps,map)
      (BaseType(n,nres._1),nres._2)
    case TOpt(t) =>
      val (nt,nm) = mkNewParametricTVar(t,map)
      (TOpt(nt),nm)
    case TEithers(f,os) =>
      val (nf,nmf) = mkNewParametricTVar(f,map)
      var current = mkNewParametricTVar(os.head,nmf)
      var nothers = List(current._1)
      for (o<-os.tail) {
        current = mkNewParametricTVar(o,current._2)
        nothers::= current._1
      }
      (TEithers(nf,nothers),current._2)
    case TTuple(f,os) =>
      val (nf,nmf) = mkNewParametricTVar(f,map)
      var current = mkNewParametricTVar(os.head,nmf)
      var nothers = List(current._1)
      for (o<-os.tail) {
        current = mkNewParametricTVar(o,current._2)
        nothers::= current._1
      }
      (TTuple(nf,nothers),current._2)
    case TProd(f,os) =>
      val (nf,nmf) = mkNewParametricTVar(f,map)
      var current = mkNewParametricTVar(os.head,nmf)
      var nothers = List(current._1)
      for (o<-os.tail) {
        current = mkNewParametricTVar(o,current._2)
        nothers::= current._1
      }
      (TProd(nf,nothers),current._2)

  }
}