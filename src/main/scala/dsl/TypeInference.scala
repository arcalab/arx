package dsl

import common.{TypeException, UndefinedVarException}

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

    def get:Map[String,TypeExpr] = ctx

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
    // - the type of the assignment, for now is the type of the id
    // - the set of constraints with the addition of Tid = Texp
    (ctxexp,T,cexp+newConst)
  }

  private def infer(ctx:Context, expr:Expr, adt:Map[Variant,TypeDecl]):(Context,TypeExpr,Set[TCons]) = {
//    var dt:Map[Variant,TypeDecl] = adt.flatMap(td => td.variants.map(v => (v -> td))).toMap
    expr match {
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
        // add a new variable to the context
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
        println("The type of the constructor name is = " + rctype)
        println("the type of the formal parameters of the constructor are"+ cctype)
        // infer the types of the actual parameters
        var pstypes = ps.map(p => infer(ctx,p,adt))
        println("the type of the actual parameters of the constructor are"+ pstypes.map(c=>c._2).mkString("\n"))
        // mk constructor constraints from each actual param to each formal param
        // todo: asumme for now we already check at parsing if the number of params match
        // todo: see how to handle TVar in this case
//        var paramsConst:Set[TCons] = mkConstCons(cctype,pstypes.map(pt => pt._2))
        var paramsConst:Set[TCons] = cctype.zip(pstypes.map(pt => pt._2)).map(p => TCons(p._1,p._2)).toSet
        println("the constraints parameter to parameter are "+ paramsConst.mkString("\n"))
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
  }


  def getFormalParamsTypes(v:Variant, adt:Map[Variant,TypeDecl]):List[TypeExpr] = v match {
    case TypeVal(n) => List()
    case TypeCons(n,ps) =>
      //find the expected type expression of each parameter
      var tp = ps.map(p => getVariantType(p))
      tp
  }


  def getVariantType(tn:TypeName):TypeExpr = tn match {
    case AbsTypeName(atn) => TVar(atn)// TVar(freshVar())
    case ConTypeName(ctn,ps) =>
      BaseType(ctn,ps.map(p => getVariantType(p)))
//      mkNewParametricTVars(res,Map())._1
  }

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

/* T1 = T2 */
case class TCons(l:TypeExpr,r:TypeExpr) {}

sealed trait TypeExpr {
  def substitute(tvar:TVar,te:TypeExpr):TypeExpr
}

/* ADT type */
case class BaseType(name:String,param:List[TypeExpr]) extends TypeExpr {
  def substitute(tvar:TVar,te:TypeExpr):BaseType = {
    BaseType(name,param.map(t => t.substitute(tvar,te)))
  }
}
/* typeExp -> typeExp */
case class TMap(from: TypeExpr, to:TypeExpr) extends TypeExpr {
  def substitute(tvar:TVar,te:TypeExpr):TMap = {
    TMap(from.substitute(tvar,te),to.substitute(tvar,te))
  }
}
/* type variable */
case class TVar(name:String) extends TypeExpr {
  def occurs(te:TypeExpr):Boolean = te match {
    case TUnit => false
    case t@TVar(n) => this == t
    case TMap(t1,t2) => this.occurs(t1) || this.occurs(t2)
    case BaseType(name, param) => param.exists(t=> this.occurs(t))
  }

  def substitute(tvar:TVar,te:TypeExpr):TypeExpr = if (this == tvar) te else this
}

case object TUnit extends TypeExpr {
  def substitute(TVar: TVar,te:TypeExpr):TypeExpr = this
}
