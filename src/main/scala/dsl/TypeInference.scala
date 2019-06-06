package dsl

import common.TypeException

//
///**
//  * Created by guillecledou on 2019-06-03
//  */
//
//
object TypeInference {

  private var tVars:Int = 0
  private def freshVar():String = {tVars+=1; (tVars-1).toString}

  class Context {
    protected val ctx: Map[String,TypeExpr] = Map()

    def contains(x:String):Boolean = ctx contains x

    def apply(x:String):TypeExpr = ctx(x)

    // todo: decide how to handle multiple definitions of a variable, for now assume that doesn't happen
    def add(x:String,t:TypeExpr):Context = {
      var oldCtx = ctx
      new Context {
          override val ctx = oldCtx + (x -> t)
        }
    }



    // todo: decide how to handle multiple definitions of a variable, for now assume that doesn't happen
    def join(other:Context):Context = {
      var oldCtx = ctx
      var newCon:Context = other
      for ((k,v) <- oldCtx) {
        newCon = newCon.add(k,v)
      }
      newCon
    }

    def get = ctx

  }


  def infer(ast:AST):(Context,TypeExpr,Set[TCons]) = {
    var adt:Map[Variant,TypeDecl] = ast.getTypes.flatMap(td => td.variants.map(v => (v -> td))).toMap
    tVars = 0
    infer(ast,adt)
  }


  def infer(ast:AST,adt:Map[Variant,TypeDecl]):(Context,TypeExpr,Set[TCons]) = {
    var assig = ast.getAssignments
    var res:(Context,TypeExpr,Set[TCons]) = (new Context,TUnit,Set[TCons]())
    var newCons:Set[TCons] = Set()
    var newCtx = new Context
    for (a <- assig) {
      res = infer(newCtx,a,adt)
      newCtx = res._1
      newCons ++= res._3
    }
    (res._1,res._2,newCons)
  }

  private def infer(ctx:Context, asg:Assignment, adt:Map[Variant,TypeDecl]/*List[TypeDecl]*/): (Context,TypeExpr,Set[TCons]) = {
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
    // - the type of the assignment, for now is the type of the id, todo: although an assignment should be of type unit?
    // - the set of constraints with the addition of Tid = Texp
    (ctxexp,T,cexp+newConst)
  }

  private def infer(ctx:Context, expr:Expr, adt:Map[Variant,TypeDecl]):(Context,TypeExpr,Set[TCons]) = {
//    var dt:Map[Variant,TypeDecl] = adt.flatMap(td => td.variants.map(v => (v -> td))).toMap
    expr match {
      case t@AdtTerm(n) => try {
        // find the type of the adt term
        var variant = adt.find(k => k._1.name == n).get._1
        var ttype = getTypeExpr(variant, adt)
        // replace all parametric types with new variables accordingly
        ttype = mkNewParametricTVars(ttype,Map())._1
        //todo: see how to handle TVar in this case
        (ctx, ttype, Set())
      } catch {
        case e:NoSuchElementException => throw new TypeException("Unknown variant name: " + n)
      }
      case c@AdtConsExpr(n, ps) => try {
        // find the type of the constructor
        var variant = adt.find(k => k._1.name == n).get._1
        var ctype = getTypeExpr(variant,adt)
        // replace all parametric types with new variables accordingly
        ctype = mkNewParametricTVars(ctype,Map())._1
        // infer the types of the actual parameters
        var pstypes = ps.map(p => infer(ctx,p,adt))
        // mk constructor constraints from each actual param to each formal param
        // todo: asumme for now we already check at parsing if the number of params match
        // todo: see how to handle TVar in this case
        var paramsConst = mkConstCons(ctype,pstypes.map(pt => pt._2))
        // mk the type of this expression:
        var T = TVar(freshVar())
        // mk the actual type of the constructor based on the infer types of the parameters
        //var actConsType = pstypes.map(pt=>pt._2) match {
        //  case List(p) => TMap(p,)
        //  case h::tail =>
        //}
        var newCons = TCons(T,ctype)
        // mk new Context from inferred context
        var newCtx:Context = pstypes.map(pt => pt._1).foldRight(ctx)(_.join(_))
        (newCtx, T, paramsConst++pstypes.flatMap(pt=> pt._3)+(newCons))
      } catch {
        case e:NoSuchElementException => throw new TypeException("Unknown variant name: " + n)
      }
    }
  }

  def mkConstCons(consType:TypeExpr,actParams:List[TypeExpr]):Set[TCons] = {
    // the last type expr of consType if the type of the constructor, thus ignore
    if (actParams.isEmpty) Set()
    else {
      consType match {
        case TMap(t1, t2) => Set(TCons(t1, actParams.head)) ++ mkConstCons(t2, actParams.tail)
        case t@TVar(n) => Set(TCons(t,actParams.head))
        case t@BaseType(n, p) => Set(TCons(t,actParams.head))
      }
    }
  }

  def getTypeExpr(v:Variant, adt:Map[Variant,TypeDecl]):TypeExpr = v match {
        case TypeVal(n) =>
          var tn = adt(v).name
          getTypeExpr(tn)
        case TypeCons(n,List(p)) =>
          // find the type name of the constructor
          var tc = getTypeExpr(adt(v).name)
          //find the expected type expression of each parameter
          var tp = getTypeExpr(p)
          // return the expected type of the constructor
          TMap(tp,tc)
        case TypeCons(n,p::ps) =>
          // find the type name of the constructor
          var tc = getTypeExpr(adt(v).name)
          //find the expected type expression of each parameter
          // type of first parameter
          var ptype = getTypeExpr(p)
          // type of rest of the parameter
          var pstypes = ps.map(p => getTypeExpr(p))
          // from right to left create the expected type of the constructor
          var res = (ptype::pstypes).foldRight(tc)(TMap(_,_))
          println(s"type exp of variant: $v = \n $res")
          res
  }

  def getTypeExpr(tn:TypeName):TypeExpr = tn match {
    case AbsTypeName(atn) => TVar(atn)// TVar(freshVar())
    case ConTypeName(ctn,ps) =>
      BaseType(ctn,ps.map(p => getTypeExpr(p)))
//      mkNewParametricTVars(res,Map())._1
  }


  def mkNewParametricTVars(te: TypeExpr,map:Map[TVar,TVar]):(TypeExpr,Map[TVar,TVar]) = te match {
    case TUnit => (TUnit,map)
    case t@TVar(n) if n.matches("[a-z][a-zA-Z0-9_]*") =>
      if (map.contains(t))
        (map(t),map)
      else {
        var fresh = TVar(freshVar())
        (fresh,map+(t->fresh))
      }
    case TMap(from, to) =>
      val (nfrom,nmapf) = mkNewParametricTVars(from,map)
      val (nto,nmapt) = mkNewParametricTVars(to,nmapf)
      (TMap(nfrom,nto),nmapt)
    case BaseType(n,ps) =>
      var params = ps.toIterator
      var nps:List[TypeExpr] = List()
      var nmap = map
      while (params.hasNext) {
        var res = mkNewParametricTVars(params.next(),nmap)
        nmap = res._2
        nps++= List(res._1)
      }
      (BaseType(n,nps),nmap)
  }
}

/* T1 = T2 */
case class TCons(l:TypeExpr,r:TypeExpr) {}

sealed trait TypeExpr

/* ADT type */
case class BaseType(name:String,param:List[TypeExpr]) extends TypeExpr
/* typeExp -> typeExp */
case class TMap(from: TypeExpr, to:TypeExpr) extends TypeExpr
/* type variable */
case class TVar(name:String) extends TypeExpr

case object TUnit extends TypeExpr