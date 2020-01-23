package dsl.analysis.types

import dsl.analysis.syntax.Program.Block
import dsl.analysis.syntax._
import dsl.common.{TypeException, UndefinedNameException}
import dsl.analysis.syntax.SymbolType._
import dsl.backend.{In, Out, Show, Simplify}

/**
  * Created by guillecledou on 2019-08-01
  */

object Infer {

  type TypeResult = (Context,TExp,Set[TCons])
  type TypeInfo = (TExp,Set[TCons])
  type ReactInfo = (RExp,Set[RCons])

  private var tVars:Int = 0
  private def freshVar():String = {tVars+=1; (tVars-1).toString}

  /**
    * Infer the type of the program expression by means of constraint typing
    * @param prog A program
    * @return A context of names, with their respective inferred type expressions,
    *         the type expression of the program,
    *         a set of type constraints to be solved
    */
  def apply(prog:Program):TypeResult = {
    // create a new context
    var ctx = Context()
    // initialize it with the predefine types and functions (perhaps this is done before? and the context is received?
    // add primitive functions
    ctx = Context(ctx.adts,mkPrimFunctions(),ctx.ports)
    // add the user defined types
    prog.types.foreach(t => ctx = addUserTypes(t,ctx))
    // infer the type of the program block
    val(pctx,pt,ptcons) = infer(prog.block,ctx)
    // check the ports context is closed //TODO: how to handle this?
//    if (!Check.isClosed(pctx.ports))
//      throw new TypeException(s"Input/Output Typing Context not closed in main program")
    // match input/output context
    val inOutTCons = portsMatch(pctx.ports)
    (pctx,Simplify(pt),ptcons++inOutTCons)
  }


  private def mkPrimFunctions():Map[String,FunEntry] = {
    Map(mkPrimFunction("fifo",1,1),
      mkPrimFunction("lossy",1,1)
      ,mkPrimFunction("drain",2,0)
      ,mkPrimFunction("dupl",1,2)
      ,mkPrimFunction("merger",2,1)
      ,mkPrimFunction("xor",1,2)
      ,mkPrimFunction("writer",0,1)
      ,mkPrimFunction("reader",1,0))

  }

  private def mkPrimFunction(name:String,ins:Int,outs:Int):(String,FunEntry) = {
    val tVar = TVar(freshVar())
    val insT = (1 to ins).map(_=>tVar).foldRight[TExp](TUnit)(TTensor(_,_))
    val outsT =(1 to outs).map(_=>tVar).foldRight[TExp](TUnit)(TTensor(_,_))
    val funT = TFun(Simplify(insT),Simplify(outsT))
    (name,FunEntry(funT,Context()))
  }


  /**
    * Add user defines to the context
    * @param typeDecl type declaration
    * @param ctx current context
    * @return
    */
  private def  addUserTypes(typeDecl:TypeDecl, ctx:Context):Context = {
    def mkUserType(typDecl:TypeDecl):(TBase,List[ConstEntry]) = {
      val tn = typeName2TExp(typDecl.name)
      val const = typDecl.constructors.map(c => ConstEntry(c.name,c.param.map(p=>typeName2TExp(p)),tn))
      (tn.asInstanceOf[TBase],const)
    }
    // create a type expression for the user defined type
    val nt = mkUserType(typeDecl)
    // check that each type param in the types name is abstract
    if (nt._1.tParams.exists(p => !p.isInstanceOf[TVar]))
      throw new RuntimeException(s"Cannot use a concrete type as a parameter to a type definition - in xx") // Show(t)
    // add the type name to the new context
    var nctx = ctx.add(typeDecl.name.name, TypeEntry(nt._1, nt._2))
    // checking that each type param exists if it is concrete type
    for (c <- nt._2) {
      c.params.foreach { case TBase(n, ps) if !nctx.adts.contains(n) =>
        throw new UndefinedNameException(s"Name $n doesn't correspond to an existing type")
      case _ => ()
      }
    }
    nctx
  }

  private def typeName2TExp(tn:TypeName):TExp = tn match {
    case ConTypeName(n,ps) => TBase(n,ps.map(p=>typeName2TExp(p)))
    case AbsTypeName(n) => TVar(n)
  }

  private def infer(block:Block,ctx:Context):TypeResult = block match  {
    case Nil => (ctx,TUnit,Set())
    case b::bs =>
      val (bCtx,bT,bTCons) = infer(b,ctx)
      val (bsCtx,bsT,bsTCons) = infer(bs,bCtx)
      (bsCtx,TTensor(bT,bsT),bTCons++bsTCons)
  }

  private def infer(st:Statement,ctx:Context):TypeResult = st match {
    case se:StreamExpr => infer(se,ctx)
    case a@Assignment(variables, expr) =>
      //check that each variable on the lhs, if it exists, is a variable (VAR) and names don't repeat
      Check.lhsAssigAreVars(variables,ctx)
      // new context
      var nctx = ctx
      // creat a fresh variable for each lhs variable
      val lhsTypes = variables.map(v=> TVar(freshVar()))
      // add each variable as an Output variable
      variables.zip(lhsTypes).foreach(v => nctx = nctx.add(v._1,PortEntry(v._2,Out)))
      // get the type of the expression
      val (ectx,et,etcons) = infer(expr,nctx)
      // create a tensor type for the lhs variables
      val lhsTTensor = Simplify(lhsTypes.foldRight[TExp](TUnit)(TTensor))
      // create a type constraint
      val tcons = TCons(lhsTTensor,et)
      (ectx,TUnit,etcons+tcons)
    case fd@FunDef(name, params, typ, block) if !ctx.context.contains(name) =>
      val insNames = params.map(i=> i.name).toSet
      if (insNames.size != params.size)
        new TypeException(s"Cannot repeat input variables on a function definition ${insNames.mkString(",")} found")
      //create a new context for the function that knows only function names and type names - no variable names
      var fctx = Context(ctx.adts,ctx.functions,Map())
      // create a type for each input port (specified or new type variable)
      val insPorts:List[(String,TExp)] = params.map(p=> (p.name,
        if (p.typ.isDefined){
          val userType = typeName2TExp(p.typ.get)
          if (ctx.adts.contains(p.typ.get.name) && Check.wellDefinedType(userType,ctx.adts(p.typ.get.name).tExp,ctx)) {
            userType
          } else
            throw new UndefinedNameException(s"Unknown type ${Show(userType)}")
          }
        else TVar(freshVar())))
      // create a fresh type variable for each specified ports whose specified type is a type variable
      val substParams = Substitution((insPorts.flatMap(p=> p._2.vars)).map(v => v->TVar(freshVar())).toMap)
//        ++(
//        if (typ.isDefined)
//          if (ctx.adts.contains(typ.get.name))
//            Set(ctx.adts(typ.get.name).tExp)
//          else throw new UndefinedNameException(s"Name ${typ.get.name} doesn't correspond to an existing type")
//        else Set()
//      )
      val freshInsPorts = insPorts.map(p=>(p._1,substParams(p._2)))
      // add to the context the data params (NOT FOR NOW) and input params
      val insTypes:List[TExp] = freshInsPorts.map(p => {fctx = fctx.add(p._1,PortEntry(p._2,In)); p._2})
      // get the type of the block
      val (bctx,bt,btcons) = infer(block,fctx)
      // check the type of the block is not a function (must be any interface type)
      val btInterfaceType = Check.isInterfaceType(Simplify(bt))
      // create the function type
      var tfun = TFun(Simplify(insTypes.foldRight[TExp](TUnit)(TTensor)),btInterfaceType)
      //// create a function entry
      //val funEntry = FunEntry(tfun,fctx) //todo: update if we eventually have recursion
      //println(s"Function type defined ${name}: "+ tfun)
      // check the ports context is closed
      Check.isClosed(fd,bctx.ports.filterNot(p=> insNames.contains(p._1)))
      // match input/output context
      val inOutTCons = portsMatch(bctx.ports)
      // unify constraints from body
      val (unified,unsolved) = Unify(btcons++inOutTCons)
      val subst:Map[TVar,TExp] = Substitute(unified)
      val substitution = Substitution(subst)
      val subsTFun = TFun(Simplify(substitution(tfun.tIn)),Simplify(substitution(tfun.tOut)))
      // create a function entry
      val funEntry = FunEntry(subsTFun,substitution(fctx)) //todo: update if we eventually have recursion
//      // if user specified type, mk a type constraint
//      val tcons = if (typ.isDefined) {
//        val utfun = substParams(ctx.adts(typ.get.name).tExp)
//        Set(TCons(utfun,funEntry.tExp.tOut))
//      }
//      val subsCtx = fctx.map(e=>e._1-> Simplify(substitution(f._2.tExp)))
      (ctx.add(name,funEntry),TUnit,unsolved)//Set())//btcons++inOutTCons)
    case FunDef(name, params, typ, block)  => // already defined
      throw new RuntimeException(s"Name $name already defined in the context")
//    TODO: case SFunDef(name, typ, sfun) =>
//      _
  }

  private def portsMatch(ports:Map[String,List[PortEntry]]):Set[TCons] =
    ports.flatMap(p=>portMatch(p._2)).toSet

  private def portMatch(pEntries:List[PortEntry]):Set[TCons] = pEntries match {
    case Nil => Set()
    case p::Nil => Set()
    case p1::p2::ps => portMatch(p2::ps)+TCons(p1.tExp,p2.tExp)
  }

  private def infer(se:StreamExpr,ctx:Context):TypeResult = se match {
    case Port(x) =>
      // create a new type variable
      val pt = TVar(freshVar())
      // add it to the context as a new Input port
      (ctx.add(x,PortEntry(pt,In)),pt,Set())
    case Const(q,args) if ctx.constructors.contains(q)  => // if the constructor exists
      // get constructor entry
      val qentry:ConstEntry = ctx.constructors(q)
      //check the number of expected parameters match
      Check.numParams(args.size,qentry.params.size)
      // create new context from known context
      var nctx = ctx
      // get the type of each actual param
      var apType:List[TypeResult] = List()
      for (a <- args) {
        val aType = infer(a,nctx)
        apType :+= aType
        nctx = aType._1
      }
      // get a fresh type for the constructor
      val subst = Substitution((qentry.params.flatMap(_.vars)++qentry.tExp.vars)
        .map(v => v->TVar(freshVar())).toMap)
      val qFType = subst(qentry.tExp)
      // fresh formal params
      val fpType = qentry.params.map(subst(_))
      // get type constraints from actual to formal params
      val tcons = apType.map(t => t._2).zip(fpType).map(p => TCons(p._1,p._2))
      // return the result, add type constraints obtained when inferring actual params type
      (nctx,qFType,apType.flatMap(t=>t._3).toSet++tcons)
    case Const(q,_) => throw new UndefinedNameException(s"Undefined Constructor name $q")
    case FunctionApp(sfun, args) =>
      // get the type of the stream function
      val sfTypeRes = infer(sfun,ctx)
//      println("Function type: "+ sfTypeRes._2)
      val sfType = Check.isFunType(sfTypeRes._2)
      // get the type of each actual param
      var nctx = ctx
      var apType:List[TypeResult] = List()
      for (a <- args) {
        val aType = infer(a,nctx)
        apType :+= aType
        nctx = aType._1
      }
      // tensor for actual params type
      val actualPsType = Simplify(apType.map(r => r._2).foldRight[TExp](TUnit)(TTensor))
      // get type constraints from actual to formal params
      val tcons  = Set(TCons(actualPsType,sfType.tIn))
      // return the result
      (nctx,sfType.tOut,apType.flatMap(r=> r._3).toSet++tcons)
  }

  private def infer(sf:StreamFun,ctx:Context):TypeResult = sf match {
    case FunName(f) if ctx.functions.contains(f) =>
      // get function entry
      val fEntry = ctx.functions(f)
      // get fresh type for the function
      val subst = Substitution(fEntry.tExp.vars.map(v => v->TVar(freshVar())).toMap)
      val fFType = subst(fEntry.tExp)
      (ctx,fFType,Set())
    case FunName(f) => //TODO: Check if it makes sense
      // special case, if it doesn't exists assume it has to be a 1->1 sync with a name
      val tVar = TVar(freshVar())
      // create dummy function type
      val ftype = TFun(tVar,tVar)
      // add dummy function to the context?
      val funEntry = FunEntry(ftype,Context(ctx.adts,ctx.functions,Map()))
      (ctx.add(f,funEntry),ftype,Set())
    case SeqFun(f1,f2) =>
      val (f1ctx,f1t,f1tcons) = infer(f1,ctx)
      val (f2ctx,f2t,f2tcons) = infer(f2,f1ctx)
      val tf1:TFun = Check.isFunType(f1t)
      val tf2:TFun = Check.isFunType(f2t)
      // create a type constraint from f1 out to f2 in
      val tcons = Set(TCons(tf1.tOut,tf2.tIn))
      val ft = TFun(tf1.tIn,tf2.tOut)
      (f2ctx,ft,f1tcons++f2tcons++tcons)
    case ParFun(f1,f2) =>
      val (f1ctx,f1t,f1tcons) = infer(f1,ctx)
      val (f2ctx,f2t,f2tcons) = infer(f2,f1ctx)
      val tf1 = Check.isFunType(f1t)
      val tf2 = Check.isFunType(f2t)
      val nInType = TTensor(tf1.tIn,tf2.tIn)
      val nOutType = TTensor(tf1.tOut,tf2.tOut)
      val ft = Simplify(TFun(nInType,nOutType))
      (f2ctx,ft,f1tcons++f2tcons)
    case Match =>
      val tVar = TVar(freshVar())
      val mtype = TFun(tVar,TDestr(tVar))
      (ctx,mtype,Set())
    case Build =>
      val tVar = TVar(freshVar())
      val btype = TFun(TDestr(tVar),tVar)
      (ctx,btype,Set())
  }

}
