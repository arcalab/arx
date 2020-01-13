package dsl.analysis.types

import dsl.analysis.syntax.Program.Block
import dsl.analysis.syntax._
import dsl.common.{TypeException, UndefinedNameException}
import dsl.analysis.syntax.SymbolType._
import dsl.backend.{In, Out, Show, Simplify}

/**
  * Created by guillecledou on 2019-08-01
  */

object Inference {

  type TypeResult = (TContext,TExp,Set[TCons])

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
    var ctx = TContext()
    // initialize it with the predefine types and functions (perhaps this is done before? and the context is received?
    // add primitive functions
    ctx = TContext(ctx.adts,mkPrimFunctions(),ctx.ports)
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
    val insT = (1 to ins).map(_=>tVar).foldRight[TExp](TUnit)(TInterface(_,_))
    val outsT =(1 to outs).map(_=>tVar).foldRight[TExp](TUnit)(TInterface(_,_))
    val funT = TFun(insT,outsT)
    (name,FunEntry(funT,TContext()))
  }


  /**
    * Add user defines to the context
    * @param typeDecl type declaration
    * @param ctx current context
    * @return
    */
  private def  addUserTypes(typeDecl:TypeDecl2, ctx:TContext):TContext = {
    def mkUserType(typDecl:TypeDecl2):(TBase,List[ConstEntry]) = {
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

  private def infer(block:Block,ctx:TContext):TypeResult = block match  {
    case Nil => (ctx,TUnit,Set())
    case b::bs =>
      val (bCtx,bT,bTCons) = infer(b,ctx)
      val (bsCtx,bsT,bsTCons) = infer(bs,bCtx)
      (bsCtx,TInterface(bT,bsT),bTCons++bsTCons)
  }

  private def infer(st:Statement,ctx:TContext):TypeResult = st match {
    case se:StreamExpr => infer(se,ctx)
    case a@Assignment2(variables, expr) =>
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
      // check that et has as many outputs as ids on the lhs of the assignment
      val exprOutTypes = et.outputs
      Check.numParams(exprOutTypes.size,variables.size)
      // create a type constraint from each returned type to each lhs variable
      val tcons = lhsTypes.zip(exprOutTypes).map(p => TCons(p._1,p._2))
      (ectx,TUnit /*et*/,etcons++tcons)
    case fd@FunDef2(name, params, typ, block) if !ctx.context.contains(name) =>
      val insNames = params.map(i=> i.name).toSet
      if (insNames.size != params.size)
        new TypeException(s"Cannot repeat input variables on a function definition ${insNames.mkString(",")} found")
      //create a new context for the function that knows only function names and type names - no variable names
      var fctx = TContext(ctx.adts,ctx.functions,Map())
      // create new type variables for each input port
      val insPorts = params.map(p=> (p.name, TVar(freshVar())))
      // add to the context the data params (NOT FOR NOW) and input params
      val insTypes:List[TExp] = insPorts.map(p => {fctx = fctx.add(p._1,PortEntry(p._2,In)); p._2})
      // get the type of the block
      val (bctx,bt,btcons) = infer(block,fctx)
      // check the type of the block is not a funtion (must be any interface type)
      val btInterfaceType = Check.isInterfaceType(Simplify(bt))
      // create the function type
      var tfun = TFun(insTypes.foldRight[TExp](TUnit)(TInterface),btInterfaceType)
//      // create a function entry
//      val funEntry = FunEntry(tfun,fctx) //todo: update if we eventually have recursion
//      println(s"Function type defined ${name}: "+ tfun)
      // create a fresh type for the function where inputs type variables are replace by the specified type, if any
      // TODO: add this later
      //val subst = Substitution(insTypes.zip(params).map(i => if (i._2.typ.isDefined) ))
      //val insTCons = insTypes.zip(params).map(i => if (i._2.typ.isDefined) TCons(i._1,i._2.typ.get)
      // check the ports context is closed
      Check.isClosed(fd,bctx.ports.filterNot(p=> insNames.contains(p._1)))
      // match input/output context
      val inOutTCons = portsMatch(bctx.ports)
      // unify constraints from body
      val subst:Map[TVar,TExp] = Substitute(Unify1(btcons++inOutTCons))
      val substitution = Substitution(subst)
      val subsTFun = TFun(substitution(tfun.tIn),substitution(tfun.tOut))
      // create a function entry
      val funEntry = FunEntry(subsTFun,substitution(fctx)) //todo: update if we eventually have recursion
//      val subsCtx = fctx.map(e=>e._1-> Simplify(substitution(f._2.tExp)))
      (ctx.add(name,funEntry),TUnit,Set())//btcons++inOutTCons)
    case FunDef2(name, params, typ, block)  => // already defined
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

  private def infer(se:StreamExpr,ctx:TContext):TypeResult = se match {
    case Port(x) =>
      // create a new type variable
      val pt = TVar(freshVar())
      // add it to the context as a new Input port
      (ctx.add(x,PortEntry(pt,In)),pt,Set())
//    case Port(x) if ctx.ports.contains(x) =>
//      // if the port is already defined and as a variable return the type of port
//        (ctx, ctx(x).get.tExp, Set())
//    case Port(x) =>
//      // if it doesn't exists as a variable, create a new type variable and try to add id to the context
//        val pt = TVar(freshVar())
//        (ctx.add(x,PortEntry(pt,VAR)),pt,Set())
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
      //check the number of expected parameters match
      Check.numParams(args.size,sfType.tIn.inputs.size)
      // get the type of each actual param
      var nctx = ctx
      var apType:List[TypeResult] = List()
      for (a <- args) {
        val aType = infer(a,nctx)
        apType :+= aType
        nctx = aType._1
      }
      // get type constraints from actual to formal params
      val tcons = apType.map(r => r._2).zip(sfType.tIn.inputs).map(p => TCons(p._1,p._2))
//      println(s"Type constraints for function application "+tcons)
      // return the result
      (nctx,sfType.tOut,apType.flatMap(r=> r._3).toSet++tcons)
  }

  private def infer(sf:StreamFun,ctx:TContext):TypeResult = sf match {
    case FunName(f) if ctx.functions.contains(f) =>
      // get function entry
      val fEntry = ctx.functions(f)
      // get fresh type for the function
//      println(s"Function type ${f}: "+ fEntry.tExp)
      val subst = Substitution(fEntry.tExp.vars.map(v => v->TVar(freshVar())).toMap)
      val fFType = subst(fEntry.tExp)
//      println(s"Fresh Function type ${f}: "+ fFType)
      (ctx,fFType,Set())
    case FunName(f) => //TODO: Check if it makes sense
      // special case, if it doesn't exists asumme it has to be a 1->1 sync with a name
      val tVar = TVar(freshVar())
      // create dummy function type
      val ftype = TFun(tVar,tVar)
      // add dummy function to the context?
      val funEntry = FunEntry(ftype,TContext(ctx.adts,ctx.functions,Map()))
      (ctx.add(f,funEntry),ftype,Set())
      //throw new UndefinedNameException(s"Undefined Function name $f")
    case SeqFun(f1,f2) =>
      val (f1ctx,f1t,f1tcons) = infer(f1,ctx)
      val (f2ctx,f2t,f2tcons) = infer(f2,f1ctx)
      val tf1:TFun = Check.isFunType(f1t)
      val tf2:TFun = Check.isFunType(f2t)
      // check number outputs from f1 match number of inputs from f2
      Check.numParams(tf1.tOut.outputs.size,tf2.tIn.inputs.size)
      // create a type constraint from each out type to each in type
      val tcons = tf1.tOut.outputs.zip(tf2.tIn.inputs).map(p=> TCons(p._1,p._2))
      val ft = TFun(tf1.tIn.inputs.foldRight[TExp](TUnit)(TInterface),tf2.tOut.outputs.foldRight[TExp](TUnit)(TInterface))
      (f2ctx,ft,f1tcons++f2tcons++tcons)
    case ParFun(f1,f2) =>
      val (f1ctx,f1t,f1tcons) = infer(f1,ctx)
      val (f2ctx,f2t,f2tcons) = infer(f2,f1ctx)
      val tf1 = Check.isFunType(f1t)
      val tf2 = Check.isFunType(f2t)
      val nInType = tf1.tIn.inputs.foldRight[TExp](tf2.tIn.inputs.foldRight[TExp](TUnit)(TInterface))(TInterface)
      val nOutType = tf1.tOut.outputs.foldRight[TExp](tf2.tOut.outputs.foldRight[TExp](TUnit)(TInterface))(TInterface)
      val ft = TFun(nInType,nOutType)
      (f2ctx,ft,f1tcons++f2tcons)
    //TODO: Build and Match
  }

}
