package dsl.analysis.semantics

import dsl.analysis.syntax._
import dsl.common.{TypeException, UndefinedVarException}
import dsl.DSL
import dsl.backend.Show
import preo.ast.CPrim
import preo.backend.Network
import preo.backend.Network.Prim


/**
  * Created by guillecledou on 2019-06-03
  */

object TypeInference {

  private var tVars:Int = 0
  private def freshVar():String = {tVars+=1; (tVars-1).toString}

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
    var adt:Map[String,TypeDecl] = ast.getTypes.flatMap(td => td.variants.map(v => v.name -> td)).toMap
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
    var tConns:Map[String,TypeConn] = conns.map(c => c.name -> TypeConn(c)).toMap
    // get assignments and multi-assignments (for now are the only expressions to type)
    var assig = ast.getAssignments
    // for each (multi)assignment infer its type, using the context from previous inferred assignments
    // accumulate all the constraints from each inferred assignment
    var res:(Context,TypeExpr,Set[TCons]) = (new Context,TUnit,Set[TCons]())
    for (a <- assig) {
      res = infer(a,ctx,tConns,adt)
      ctx = res._1
      tCons ++= res._3
    }
    // return the last known context, the type of the last assignment and the accumulated set of constraints
    (res._1,tConns,res._2,tCons)
  }

  /**
    * Given an assignment ids = expression, infer the type of the ids and the expr
    * by means of constraint typing, and depending by the type of the assignment (simple, multi)
    *
    * @param asg the assignment
    * @param ctx known variables and their types
    * @param adt known user defined types
    * @return a new constraint typing relation: (context, type of the assignment, set of type constraints)
    */
  private def infer(asg:Assignment, ctx:Context, ctxConn:Map[String,TypeConn]
                    , adt:Map[String,TypeDecl]): (Context,TypeExpr,Set[TCons]) = {
    // infer based on simple or multiple assignment
    if (asg.variables.size>1)
      inferMultAsg(asg,ctx,ctxConn,adt)
    else
      inferSimpleAsg(asg,ctx,ctxConn,adt)
  }

  /**
    * Given a simple assingment infers the type of the id and the type of the expression
    * @param asg simple assignment
    * @param ctx known variables and their types
    * @param tConns known connectors with their type
    * @param adt known user defined types
    * @return a new constraint typing relation: (context, type of the id, set of type constraints)
    */
  private def inferSimpleAsg(asg:Assignment
                             ,ctx:Context,tConns:Map[String,TypeConn]
                             ,adt:Map[String,TypeDecl]): (Context,TypeExpr,Set[TCons]) = {
    // asg: id = expr
    val id:Identifier = asg.variables.head
    val expr: Expr = asg.expr
    // create a new Type Variable for id
    val T = TVar(freshVar())
    // add to the context that id is of type T
    var newCtx = ctx.add(id.name, T)
    // infer the type of the expression, with the new context
    val (ctxexp, texp, cexp) = infer(expr,newCtx, tConns, adt)
    // create a new constraint for T = texp
    val newConst = TCons(T, texp)
    // return:
    // - the new context return by the exprs (already contains newCtx)
    // - the type of the assignment, for now is the type of the id
    // - the set of constraints with the addition of Tid = Texp
    (ctxexp, T, cexp + newConst)
  }

  /**
    * Given a multiple assginment expression, find the type of the expression (connector, eventually a function)?
    * and assign to each variable on the LHS the type of the corresponding output of the connector
    * @param masg multiple assignment
    * @param ctx known variables and their types
    * @param tconns known connectors and their type
    * @param adt known user defined types
    * @return a new constraint typing relation: (context, type of the expression, set of type constraints)
    *         where the type of the expression is unit, since each variable on the LHS is already typed in the new context
    */
  private def inferMultAsg(masg:Assignment
                    ,ctx:Context,tconns:Map[String,TypeConn]
                    ,adt:Map[String,TypeDecl]):(Context,TypeExpr,Set[TCons]) = {
    // masg: v1,...,vn = connId(...)
    val ids = masg.variables
    // at parsing time we know the expression of a mult-assignment is a connId todo: or a function
    val conn  = masg.expr.asInstanceOf[ConnId]
    // infer the type of the connector
    val (ctxConn,tConn,cConn,tOuts) = inferConn(conn,ctx,tconns,adt)
    // create new constraints
    var newConst = cConn
    // create new context
    var newCtx = ctxConn
    // for each variable create a fresh variable and add it to the context
    var idsTvars:List[TVar] = for (i <-ids) yield {
      var Ti = TVar(freshVar())
      newCtx = newCtx.add(i.name,Ti)
      Ti
    } // for each variable on the LHD create a constraint to the type of the corresponding output
    newConst++= idsTvars.zip(tOuts).map(vt => TCons(vt._1,vt._2))
    // return the new context, the type of the expression, not important in this case, and the new constraints
    (newCtx,TUnit,newConst)
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
  private def infer(expr:Expr, ctx:Context, tConns:Map[String,TypeConn]
                    ,adt:Map[String,TypeDecl]):(Context,TypeExpr,Set[TCons]) = expr match {
    case Identifier(n) =>
      // if id is defined already return the type of id, other wise it is undefined
      if (ctx.contains(n))
        (ctx,ctx(n),Set())
      else throw new UndefinedVarException("Unknown identifier: " + n)
    case t@AdtTerm(n) =>
      // find the type of the adt term, we know it exists because it was checked at parsing time
      var ttype = getVariantType(adt(n).name)
      // replace all parametric types with new variables accordingly
      ttype = mkNewTypeVar(ttype,Map())._1
      // create a new variable
      var fresh = TVar(freshVar())
      // add the corresponding constraint
      var tcons = TCons(fresh,ttype)
      //todo: see how to handle TVar in this case
      (ctx, fresh, Set(tcons))
    case c@AdtConsExpr(n, ps)  =>
      // find the type of the constructor, we know it exists because it was checked at parsing time
      var rctype = getVariantType(adt(n).name)
      // find the type of the parameters
      var ctype = getFormalParamType(n,adt)
      // replace all parametric types with new variables accordingly in the parameters and in the type of the constructor
      val (cctype,m) = mkNewTypeVars(ctype,Map())//._1
      rctype = mkNewTypeVar(rctype,m)._1
      // infer the types of the actual parameters
      var pstypes = ps.map(p => infer(p,ctx,tConns,adt))
      // mk constructor constraints from each actual param to each formal param
      var paramsConst:Set[TCons] = cctype.zip(pstypes.map(pt => pt._2)).map(p => TCons(p._1,p._2)).toSet
      // mk the type of this expression:
      var T = TVar(freshVar())
      // mk new constrait for T and for the fresh variable
      var newCons = TCons(T,rctype)
      // mk new Context from inferred context
      var newCtx:Context = pstypes.map(pt => pt._1).foldRight(ctx)(_.join(_))
      (newCtx, T, paramsConst++pstypes.flatMap(pt=> pt._3)+(newCons))
    case c@ConnId(n,ps) =>
      val (newCtx,tConn,newConst,tOuts) = inferConn(c,ctx,tConns,adt)
      (newCtx,tConn,newConst)
  }

  /**
    * Given a connector id and actual params, infer the concrete type of the connector
    * and return as well the type of each of ouputs
    * @param connId connector call
    * @param ctx known variables and their types
    * @param tConns known connectors and their types
    * @param adt known user defined types
    * @return a new constraint typing relation + type of outputs:
    *         (context, type of the expression, set of type constraints, type of each output)
    */
  private def inferConn(connId:ConnId
                    ,ctx:Context, tConns:Map[String,TypeConn]
                    ,adt:Map[String,TypeDecl]):(Context,TypeExpr,Set[TCons],List[TypeExpr]) = {
    // connId: ConnId(n,ps)
    val n = connId.name
    val ps = connId.params
    // find the type of the connector with name n
    var tconn:TypeConn = tConns(n)
    // if there are more actual params than formal => error
    if (ps.size> tconn.paramSize)
      throw new TypeException(s"Connector ${n} expects no more than ${tconn.paramSize} params but ${ps.size} found")
    //create new context
    var newCtx = ctx
    // find actual output params
    var actualOuts = ps.drop(tconn.ins.size)
    // check that they are variables, and if they are not in the context, add them with a fresh type var
    var actualOutsVars = List[TVar]()
    if (actualOuts.forall(_.isVariable)) {
      actualOutsVars = actualOuts.map(_ => TVar(freshVar())) // fresh type variables for each output
      actualOuts.zip(actualOutsVars).foreach(et => newCtx = newCtx.add(et._1.asInstanceOf[Identifier].name,et._2))
    } else throw new TypeException(s"Output params must be variables, in: ${Show(connId)}")
    // todo: if it has no parameters, assume no concrete data is sent ~~ unit  (for now they remain parametric)
    // find the type of each actual input parameter of the connector
    var psInsTypes = ps.take(tconn.ins.size).map(p => infer(p,ctx,tConns,adt))
    // create new variables for each abstract (parametric) type in the formal parameters
    var renameInsTypes= mkNewTypeVars(tconn.ins,Map())
    var renameOutsTypes = mkNewTypeVars(tconn.outs,renameInsTypes._2)
    // add a constraint from each formal param to each actual param (if actual params shorter, the rest remain parametric)
    // inputs first
    var paramConst:Set[TCons] = renameInsTypes._1.zip(psInsTypes.map(pt=>pt._2)).map(p => TCons(p._1,p._2)).toSet
    // outputs then
    paramConst ++= renameOutsTypes._1.zip(actualOutsVars).map(pt => TCons(pt._2,pt._1))
    // mk a fresh var for the type of this expression
    var T = TVar(freshVar())
    // make the type of the connector (type of the output)
    var connType = TypeConn(renameInsTypes._1,renameOutsTypes._1).getOutputType//tconn.getType
    // mk a new constraint saying that the type of this expression is of type T
    var newCons = paramConst++Set(TCons(T,connType))++psInsTypes.flatMap(pt=>pt._3)
    // return the ctx, the type of the expression, the new constraints, and the type of each output
    (newCtx,T,newCons,renameOutsTypes._1)
  }


  /**
    * Given a ADT variant, return the type of its formal parameters, if any
    *
    * @param v variant
    * @param adt known user defined types
    * @return
    */
  def getFormalParamType(v:String, adt:Map[String,TypeDecl]):List[TypeExpr] = adt(v).variants.find(_.name==v).get match {
    case AdtVal(n) => List()
    case AdtConst(n,ps) =>
      //find the expected type expression of each parameter
      ps.map(p => getVariantType(p))
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
    * creates a new type variable for each abstract type, and replace each
    * appearance of the abstract type with the corresponding variable
    * E.g.: List<a> -> List<T> for some new T
    *
    * @param tes
    * @param map
    * @return
    */
  def mkNewTypeVars(tes:List[TypeExpr], map:Map[TVar,TVar]):(List[TypeExpr],Map[TVar,TVar]) = {
    var nmap = map
    var ntes = List[TypeExpr]()
    for (te <- tes) {
      var res = mkNewTypeVar(te,nmap)
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
  def mkNewTypeVar(te: TypeExpr, map:Map[TVar,TVar]):(TypeExpr,Map[TVar,TVar]) = te match {
    case TUnit => (TUnit,map)
    case t@TVar(n) if n.matches("[a-z][a-zA-Z0-9_]*") =>
      if (map.contains(t))
        (map(t),map)
      else {
        var fresh = TVar(freshVar())
        (fresh,map+(t->fresh))}
    case TMap(from, to) =>
      val (nfrom,nmapf) = mkNewTypeVar(from,map)
      val (nto,nmapt) = mkNewTypeVar(to,nmapf)
      (TMap(nfrom,nto),nmapt)
    case BaseType(n,ps) =>
      var nres = mkNewTypeVars(ps,map)
      (BaseType(n,nres._1),nres._2)
    case TOpt(t) =>
      val (nt,nm) = mkNewTypeVar(t,map)
      (TOpt(nt),nm)
    case TEithers(f,os) =>
      val (nf,nmf) = mkNewTypeVar(f,map)
      var current = mkNewTypeVar(os.head,nmf)
      val rest:List[TypeExpr] = current._1::os.map(o => {current = mkNewTypeVar(o,current._2); current._1})
      (TEithers(nf,rest),current._2)
    case TTuple(f,os) =>
      val (nf,nmf) = mkNewTypeVar(f,map)
      var current = mkNewTypeVar(os.head,nmf)
      val rest:List[TypeExpr] = current._1::os.map(o => {current = mkNewTypeVar(o,current._2); current._1})
      (TTuple(nf,rest),current._2)
    case TProd(f,os) =>
      val (nf,nmf) = mkNewTypeVar(f,map)
      var current = mkNewTypeVar(os.head,nmf)
      val rest:List[TypeExpr] = current._1::os.map(o => {current = mkNewTypeVar(o,current._2); current._1})
      (TProd(nf,rest),current._2)
  }
}