package dsl.analysis.semantics

import dsl.analysis.syntax._
import dsl.common.{TypeException, UndefinedVarException}

/**
  * Created by guillecledou on 2019-06-03
  */

object TypeInference {

  private var tVars:Int = 0
  private def freshVar():String = {tVars+=1; (tVars-1).toString}

  class Context {
    protected val ctx: Map[String,TVar] = Map()

    def get:Map[String,TVar] = ctx

    def contains(x:String):Boolean = ctx contains x

    def apply(x:String):TVar = ctx(x)

    // todo: handle multiple definitions of a variable, for now assume it doesn't happen
    def add(x:String,t:TVar):Context = {
      var oldCtx = ctx
      new Context {
          override val ctx = oldCtx + (x -> t)
        }
    }
    // todo: handle multiple definitions of a variable, for now we don't have different scopes
    def join(other:Context):Context = {
      var oldCtx = ctx
      var newCon:Context = other
      for ((k,v) <- oldCtx) {
        newCon = newCon.add(k,v)
      }
      newCon
    }
  }

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
  private def infer(ast:AST):(Context,TypeExpr,Set[TCons]) = {
    // find known adt from the ast
    var adt:Map[Variant,TypeDecl] = ast.getTypes.flatMap(td => td.variants.map(v => v -> td)).toMap
    // initialize type variables
    tVars = 0
    // get all assignments (for now are the only expressions to type
    var assig = ast.getAssignments
    // for each assignment infer its type, using the contex from previous inferred assignments
    // accumulate all the constraints from each inferred assignment
    var res:(Context,TypeExpr,Set[TCons]) = (new Context,TUnit,Set[TCons]())
    var newCons:Set[TCons] = Set()
    var newCtx = new Context
    for (a <- assig) {
      res = infer(newCtx,a,adt)
      newCtx = res._1
      newCons ++= res._3
    }
    // return the last known context, the type of the last assignment and the accumulated set of constraints
    (res._1,res._2,newCons)
  }

  /**
    * Given an assignment id = expression, finds the type of the id and the expr
    * by means of constraint typing
    *
    * @param ctx
    * @param asg
    * @param adt
    * @return a new constraint typing relation: (context, type of the id, set of type constraints)
    */
  private def infer(ctx:Context, asg:Assignment, adt:Map[Variant,TypeDecl]): (Context,TypeExpr,Set[TCons]) = {
    // asg: id = expr
    val id:Identifier = asg.variable
    val expr:Expr = asg.expr

    // create a new Type Variable for id
    val T = TVar(freshVar())
    // add to the context that id is of type T
    var newCtx = ctx.add(id.name,T)
    // infer the type of the expression, with the new context
    val (ctxexp,texp,cexp) = infer(newCtx,expr,adt)
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
    * @param asg
    * @param adt
    * @return a new constraint typing relation: (context, type of the expression, set of type constraints)
    */
  private def infer(ctx:Context, expr:Expr, adt:Map[Variant,TypeDecl]):(Context,TypeExpr,Set[TCons]) = expr match {
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
      var pstypes = ps.map(p => infer(ctx,p,adt))
      // mk constructor constraints from each actual param to each formal param
      // todo: asumme for now we already check at parsing if the number of params match
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
  }


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
  }
}