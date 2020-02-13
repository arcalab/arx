package dsl.analysis.types

import dsl.DSL
import dsl.analysis.syntax.Program.{Block, MaybeTypeName}
import dsl.analysis.syntax._
import dsl.common.{TypeException, UndefinedNameException}
import dsl.analysis.syntax.SymbolType._
import dsl.analysis.types.TProgram.TBlock
import dsl.backend._

/**
  * Created by guillecledou on 2019-08-01
  */

object Infer {

  type TypeResult =   (Context,TExp,Set[TCons],TProgram)
  type GTTypeResult = (Context,TExp,Set[TCons],TGroundTerm)
  type SFTypeResult = (Context,TExp,Set[TCons],TStreamFun)
  type SETypeResult = (Context,TExp,Set[TCons],TStreamExpr)
  type STypeResult  = (Context,TExp,Set[TCons],TStatement)
  type BTypeResult  = (Context,TExp,Set[TCons],TBlock)

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
    // add primitive functions
    ctx = Context(ctx.adts,importPrimFuns(),ctx.ports)
    // load imports
    ctx = loadImports(prog.imports,ctx)//Context()
    // add the user defined types
    prog.types.foreach(t => ctx = addUserTypes(t,ctx))
    // infer the type of the program block
    val(pctx,pt,ptcons,tb) = infer(prog.block,ctx)
    // check the ports context is closed //TODO: how to handle this?
//    if (!Check.isClosed(pctx.ports))
//      throw new TypeException(s"Input/Output Typing Context not closed in main program")
    // match input/output context
    val inOutTCons = portsMatch(pctx.ports)
    (pctx,Simplify(pt),ptcons++inOutTCons,TProgram(prog.imports,prog.types,tb))
  }

  private def loadImports(imp:List[Import],ctx:Context):Context = {
    val content:List[ModuleContent] = imp.flatMap(i=>Prelude.getImport(i))
    loadContent(content,ctx)
  }

  private def loadContent(mc:List[ModuleContent],ctx:Context):Context = mc match {
    case Nil => ctx
    case PrimType(n,td)::ls =>
      val nctx = loadContent(ls,ctx)
      addUserTypes(td,nctx)
    case PrimFun(n,sb)::ls =>
      val nctx = loadContent(ls,ctx)
      nctx.add(n,mkPrimFunEntry(PrimFun(n,sb))._2)
    case ComplFun(n,fd)::ls =>
      val nctx = loadContent(ls,ctx)
      val (nfctx,ft,cons,tfd) = infer(fd,nctx)
      nfctx
  }

  private def importPrimFuns():Map[String,FunEntry] =
    DSL.prelude.importPrimFunctions().map(mkPrimFunEntry).toMap

  private def mkPrimFunEntry(fun:PrimFun):(String,FunEntry) = {
    val tVar = TVar(freshVar())
    val insT = (1 to fun.sb._1.inputs.size).map(_=>tVar).foldRight[TExp](TUnit)(TTensor(_,_))
    val outsT =(1 to fun.sb._1.outputs.size).map(_=>tVar).foldRight[TExp](TUnit)(TTensor(_,_))
    val funT = TFun(Simplify(insT),Simplify(outsT))
    (fun.name,FunEntry(funT,Context()))
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

  private def infer(block:Block,ctx:Context):BTypeResult = block match  {
    case Nil => (ctx,TUnit,Set(),List())
    case b::bs =>
      val (bCtx,bT,bTCons,bTb) = infer(b,ctx)
      val (bsCtx,bsT,bsTCons,bsTb) = infer(bs,bCtx)
      (bsCtx,TTensor(bT,bsT),bTCons++bsTCons,bTb::bsTb)
  }

  private def infer(st:Statement,ctx:Context):STypeResult = st match {
    case se:StreamExpr => infer(se,ctx)
    case a@Assignment(variables, expr) =>
      //check that each variable on the lhs, if it exists, is a variable (VAR) and names don't repeat
      TypeCheck.lhsAssigAreVars(variables,ctx)
      // new context
      var nctx = ctx
      // creat a fresh variable for each lhs variable
      val lhsTypes = variables.map(v=> TVar(freshVar()))
      // add each variable as an Output variable
      variables.zip(lhsTypes).foreach(v => nctx = nctx.add(v._1,PortEntry(v._2,Out)))
      // get the type of the expression
      val (ectx,et,etcons,ete) = infer(expr,nctx)
      // create a tensor type for the lhs variables
      val lhsTTensor = Simplify(lhsTypes.foldRight[TExp](TUnit)(TTensor))
      // create a type constraint
      val tcons = TCons(lhsTTensor,et)
      (ectx,TUnit,etcons+tcons,TAssignment(a,lhsTypes,ete))
    case fd@FunDef(name, params, typ, block) if !ctx.context.contains(name) =>
      val insNames = params.map(i=> i.name).toSet
      if (insNames.size != params.size)
        new TypeException(s"Cannot repeat input variables on a function definition ${insNames.mkString(",")} found")
      //create a new context for the function that knows only function names and type names - no variable names
      var fctx = Context(ctx.adts,ctx.functions,Map())
      // create a type for each input port (specified or new type variable)
      val insPorts:List[(String,TExp)] = params.map(p=> (p.name, getSecifiedType(p.typ,ctx)))
      // create a type for the function (specified or new type variable)
      val specifiedFType = getSecifiedType(typ,ctx)
      // create a fresh type variable for each specified ports whose specified type is a type variable
      // plus, if the function has a specified type, use it it as well
      val substParams = Substitution((insPorts.flatMap(p=> p._2.vars)++specifiedFType.vars)
        .map(v => v->TVar(freshVar())).toMap)
      val freshInsPorts = insPorts.map(p=>(p._1,substParams(p._2)))
      // add to the context the data params (NOT FOR NOW) and input params
      val insTypes:List[TExp] = freshInsPorts.map(p => {fctx = fctx.add(p._1,PortEntry(p._2,In)); p._2})
      // get the type of the block
      val (bctx,bt,btcons,btb) = infer(block,fctx)
      // check the type of the block is not a function (must be any interface type)
      val btInterfaceType = TypeCheck.isInterfaceType(Simplify(bt))
      // create the function type
      var tfun = TFun(Simplify(insTypes.foldRight[TExp](TUnit)(TTensor)),btInterfaceType)
      //// create a function entry
      //val funEntry = FunEntry(tfun,fctx) //todo: update if we eventually have recursion
      // check the ports context is closed
      TypeCheck.isClosed(fd,bctx.ports.filterNot(p=> insNames.contains(p._1)))
      // match input/output context
      val inOutTCons = portsMatch(bctx.ports)
      // unify constraints from body ++ ports constrains ++ constraint that tfun must match specified fun type
      val substitution = TypeCheck.solve(btcons++inOutTCons++Set(TCons(tfun.tOut,specifiedFType)),ctx)
      val subsTFun = TFun(Simplify(substitution(tfun.tIn)),Simplify(substitution(tfun.tOut)))
      // create a function entry
      val funEntry = FunEntry(subsTFun,substitution(fctx)) //todo: update if we eventually have recursion
      (ctx.add(name,funEntry),TUnit,Set(),TFunDef(fd,funEntry.tExp,substitution(btb,bctx)))//Set())//btcons++inOutTCons)
    case FunDef(name, params, typ, block)  => // already defined
      throw new RuntimeException(s"Name $name already defined in the context")
//    TODO: case SFunDef(name, typ, sfun) =>
//      _
  }

  private def getSecifiedType(tn:MaybeTypeName,ctx:Context):TExp = tn match {
    case Some(t) if ctx.adts.contains(t.name) =>
      val specified = typeName2TExp(t)
      TypeCheck.wellDefinedType(specified,ctx.adts(t.name).tExp,ctx)
      specified
    case Some(t) => throw new UndefinedNameException(s"Unknown type ${Show(t)}")
    case _ => TVar(freshVar())
  }

  private def portsMatch(ports:Map[String,List[PortEntry]]):Set[TCons] =
    ports.flatMap(p=>portMatch(p._2)).toSet

  private def portMatch(pEntries:List[PortEntry]):Set[TCons] = pEntries match {
    case Nil => Set()
    case p::Nil => Set()
    case p1::p2::ps => portMatch(p2::ps)+TCons(p1.tExp,p2.tExp)
  }

  private def infer(gt:GroundTerm,ctx:Context):GTTypeResult = gt match {
    case Port(x) =>
      // create a new type variable
      val pt = TVar(freshVar())
      // add it to the context as a new Input port
      (ctx.add(x,PortEntry(pt,In)),pt,Set(),TPort(x,pt))
    case const@Const(q,args) if ctx.constructors.contains(q)  => // if the constructor exists
      // get constructor entry
      val qentry:ConstEntry = ctx.constructors(q)
      //check the number of expected parameters match
      TypeCheck.numParams(args.size,qentry.params.size)
      // create new context from known context
      var nctx = ctx
      // get the type of each actual param
      var apType:List[GTTypeResult] = List()
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
      (nctx,qFType,apType.flatMap(t=>t._3).toSet++tcons,TConst(const,qFType,apType.map(_._4)))
    case Const(q,_) => throw new UndefinedNameException(s"Undefined Constructor name $q")
  }

  private def infer(se:StreamExpr,ctx:Context):SETypeResult = se match {
    case gt:GroundTerm => infer(gt,ctx)
    case FunctionApp(sfun, args) =>
      // get the type of the stream function
      val sfTypeRes:SFTypeResult = infer(sfun,ctx)
//      println("Function type: "+ sfTypeRes._2)
      val sfType = TypeCheck.isFunType(sfTypeRes._2)
      // get the type of each actual param
      var nctx = ctx
      var apType:List[GTTypeResult] = List()
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
      (nctx,sfType.tOut,apType.flatMap(r=> r._3).toSet++tcons,TFunApp(sfTypeRes._4,sfType.tOut,apType.map(_._4)))
  }

  private def infer(sf:StreamFun,ctx:Context):SFTypeResult = sf match {
    case FunName(f) if ctx.functions.contains(f) =>
      // get function entry
      val fEntry = ctx.functions(f)
      // get fresh type for the function
      val subst = Substitution(fEntry.tExp.vars.map(v => v->TVar(freshVar())).toMap)
      val fFType = subst(fEntry.tExp)
      (ctx,fFType,Set(),TFunName(f,fFType))
    case FunName(f) => //TODO: Check if it makes sense
      // special case, if it doesn't exists assume it has to be a 1->1 sync with a name
      val tVar = TVar(freshVar())
      // create dummy function type
      val ftype = TFun(tVar,tVar)
      // add dummy function to the context?
      val funEntry = FunEntry(ftype,Context(ctx.adts,ctx.functions,Map()))
      (ctx.add(f,funEntry),ftype,Set(),TFunName(f,ftype))
    case SeqFun(f1,f2) =>
      val (f1ctx,f1t,f1tcons,sft1) = infer(f1,ctx)
      val (f2ctx,f2t,f2tcons,sft2) = infer(f2,f1ctx)
      val tf1:TFun = TypeCheck.isFunType(f1t)
      val tf2:TFun = TypeCheck.isFunType(f2t)
      // create a type constraint from f1 out to f2 in
      val tcons = Set(TCons(tf1.tOut,tf2.tIn))
      val ft = TFun(tf1.tIn,tf2.tOut)
      (f2ctx,ft,f1tcons++f2tcons++tcons,TSeqFun(sft1,sft2))
    case ParFun(f1,f2) =>
      val (f1ctx,f1t,f1tcons,sft1) = infer(f1,ctx)
      val (f2ctx,f2t,f2tcons,sft2) = infer(f2,f1ctx)
      val tf1 = TypeCheck.isFunType(f1t)
      val tf2 = TypeCheck.isFunType(f2t)
      val nInType = TTensor(tf1.tIn,tf2.tIn)
      val nOutType = TTensor(tf1.tOut,tf2.tOut)
      val ft = Simplify(TFun(nInType,nOutType))
      (f2ctx,ft,f1tcons++f2tcons,TParFun(sft1,sft2))
    case Match =>
      val tVar = TVar(freshVar())
      val mtype = TFun(tVar,TDestr(tVar))
      (ctx,mtype,Set(),TMatch(tVar,TDestr(tVar)))
    case Build =>
      val tVar = TVar(freshVar())
      val btype = TFun(TDestr(tVar),tVar)
      (ctx,btype,Set(),TBuild(TDestr(tVar),tVar))
  }

}
