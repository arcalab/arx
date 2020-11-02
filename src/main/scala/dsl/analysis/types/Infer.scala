package dsl.analysis.types

import dsl.analysis.semantics._
import dsl.analysis.syntax.Program.{Block, MaybeTypeName}
import dsl.analysis.syntax._
import dsl.analysis.types.Context.PortCtx
import dsl.analysis.types.TProgram.TBlock
import dsl.backend._
import dsl.common._

import scala.annotation.tailrec

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
    var ctx = Context()
    ctx = loadPrelude(ctx)
    ctx = loadImports(prog.imports,ctx)
    ctx = loadTypeDefinitions(prog.types,ctx)
    val(pctx,pt,ptcons,tb) = infer(prog.block,ctx)
    // match input/output context
    val inOutTCons = matchPorts(pctx.ports)
    // add inputs as program input type
    val openInsType = filterInputOnlyPorts(pctx.ports).map(p=>p._2.head.tExp)
    //pctx.ports.filter(p=> !p._2.exists(p => p.io == Out))
    //.map(p=> p._2.head.tExp)
    //val openInsType = inputPorts(pctx.ports).map(p=>p._2.texp)
    val programType = Simplify(TFun(Simplify(openInsType.foldRight[TExp](TUnit)(TTensor)),pt))
    // add the program to the context
    val npctx = pctx.add("Program",FunEntry(programType.asInstanceOf[TFun], ctx))
    (npctx,programType,ptcons++inOutTCons,TProgram(prog.imports,prog.types,tb))
  }

  /**
    * Given a context of ports it filters out ports that are mixed or output.
    * @param ports a context of ports
    * @return a context of ports that act only as inputs
    */
  private def filterInputOnlyPorts(ports:PortCtx):PortCtx =
    ports.filterNot(p => p._2.exists(entry=> entry.io == Out))

  private def matchPorts(ports:Map[String,List[PortEntry]]):Set[TCons] =
    ports.flatMap(p=>matchPort(p._2)).toSet

  private def matchPort(pEntries:List[PortEntry]):Set[TCons] = pEntries match {
    case Nil => Set()
    case p::Nil => Set()
    case p1::p2::ps => matchPort(p2::ps)+TCons(p1.tExp,p2.tExp)
  }

  /**
    * Load primitive types and functions (connectors)
    * @param ctx a context
    * @return a new context containing ctx + primitive types and functions
    */
  private def loadPrelude(ctx:Context):Context =
    loadContent(Prelude.importPrimTypes()++Prelude.importPrimFunctions(),ctx)

  /**
    * Load content imported by the user
    * @param imports a list of import declarations
    * @param ctx a context
    * @return a new context containing ctx + types and functions imported in imports
    */
  private def loadImports(imports:List[Import],ctx:Context):Context = {
    var importedContent = imports.flatMap(i => Prelude.getImport(i))
    loadContent(importedContent,ctx)
  }

  /**
    * Load types defined by the user
    * todo: check if fold is correct
    * @param types a list of type declarations
    * @param ctx a context
    * @return a new context containing ctx + new types declared by the user
    */
  private def loadTypeDefinitions(types:List[TypeDecl],ctx:Context):Context =
    types.foldRight(ctx) { (t,lc) => loadTypeDefinition(t,lc)}

  /**
    * Load a type defined by the user to the context
    * @param tdef a type declaration
    * @param ctx current context
    * @return a new context containing ctx + the new type declared
    */
  private def loadTypeDefinition(tdef:TypeDecl,ctx:Context):Context = {
    val typ = mkType(tdef.name).asInstanceOf[TBase]
    val constTypes:Map[String,ConstEntry] = tdef.constructors.map(c =>
      c.name -> ConstEntry(c.name,c.param.map(mkType),typ).setPos(c.pos)).toMap
    var nctx = ctx.add(tdef.name.name,TypeEntry(constTypes.keys.toList,typ).setPos(tdef.pos))
    if (! wellDefinedType(typ))
      throw new InvalidType(s"Type parameters must be abstract in ${tdef.pos} ")
    val illConstructor = constTypes.values.find(!wellDefinedConstructor(_,nctx))
    if (illConstructor.isDefined)
      throw new InvalidType(s"Type name ${illConstructor.get.name} not found in ${illConstructor.get.pos}")
    constTypes.foreach(c=>{nctx = nctx.add(c._1,c._2)})
    nctx
  }

  /**
    * Checks if a type is well defined:
    * - it can be represented as a base type
    * - all is type params, if any, are abstract
    * @param texp type expression to check
    * @return whether it is well defined
    */
  private def wellDefinedType(texp: TExp):Boolean = texp match {
    case TBase(_,params) => params.forall({case p:TVar => true; case _=>false})
    case _ => false
  }

  /**
    * Checks if a constructor definition is well defined:
    * - all its concrete type params, if any, are defined in the context
    * @param const constructor entry
    * @param ctx the current context
    * @return whether it is well defined
    */
  private def wellDefinedConstructor(const:ConstEntry,ctx:Context):Boolean =
    const.paramsType.forall({case p:TBase => ctx.hasType(p.name); case _=> true})


//  DEPRECATED
//  private def  addUserType(typeDecl:TypeDecl, ctx:Context):Context = {
//    def mkUserType(typDecl:TypeDecl):(TBase,List[ConstEntry]) = {
//      val tn = typeName2TExp(typDecl.name)
//      val const = typDecl.constructors.map(c => ConstEntry(c.name,c.param.map(p=>typeName2TExp(p)),tn))
//      (tn.asInstanceOf[TBase],const)
//    }
//    // create a type expression for the user defined type
//    val nt = mkUserType(typeDecl)
//    // check that each type param in the types name is abstract
//    if (nt._1.tParams.exists(p => !p.isInstanceOf[TVar]))
//      throw new RuntimeException(s"Cannot use a concrete type as a parameter to a type definition - in xx") // Show(t)
//    // add the type name to the new context
//    var nctx = ctx.add(typeDecl.name.name, TypeEntry(nt._1, nt._2))
//    // checking that each type param exists if it is concrete type
//    for (c <- nt._2) {
//      c.params.foreach { case TBase(n, ps) if !nctx.adts.contains(n) =>
//        throw new UndefinedNameException(s"Name $n doesn't correspond to an existing type")
//      case _ => ()
//      }
//    }
//    nctx
//  }

  /**
    * Creates an appropriate type expression from a TypeName (abstract or concrete)
    * @param tn a type name
    * @return the type expression for the type name
    */
  private def mkType(tn:TypeName):TExp = tn match {
    case ConTypeName(name,params) => TBase(name,params.map(p=>mkType(p)))
    case AbsTypeName(name) => TVar(name)
  }

// DEPRECATED
//  private def loadImportsAndPrims(imp:List[Import],ctx:Context):Context = {
//    val mc:List[ModuleContent] = imp.flatMap(i=>Prelude.getImport(i))
//
//    val primTypes = Prelude.importPrimTypes() ++ mc.collect({case p:PrimType => p})
//    val primFuns = mc.collect({case p:PrimFun => p})
//    val complexFun = mc.collect({case p:ComplFun => p})
//
//    val tctx = loadContent(primTypes,ctx)
//    val pfctx = Context(tctx.adts,importPrimFuns(tctx),tctx.ports,tctx.vars)
//    val fctx = loadContent(primFuns,pfctx)
//    val cfctx = loadContent(complexFun,fctx)
//    cfctx
//  }

  /**
    * Loads a list of module contents [[ModuleContent]] into a context
    *
    * @param mc list of module contents
    * @param ctx context
    * @return a new context containing ctx + the specified content
    */
  @tailrec
  private def loadContent(mc:List[ModuleContent],ctx:Context):Context = mc match {
    case Nil => ctx
    case PrimType(n,td)::ls =>
      val nctx = loadTypeDefinition(td,ctx)
      loadContent(ls,nctx)
    case PrimFun(n,sb,params)::ls =>
      val nctx = ctx.add(n,mkPFun(PrimFun(n,sb,params)))
      loadContent(ls,nctx)
    case ComplFun(_,fd)::ls =>
      val (nfctx,_,_,_) = infer(fd,ctx)
      loadContent(ls,nfctx)
  }

//  private def importPrimFuns(ctx:Context):Map[String,FunEntry] =
//    DSL.prelude.importPrimFunctions().map(mkPrimFunEntry(_,ctx)).toMap

  /**
    * Creates a function entry for a primitive function
    * @param fun primitive function
    * @return
    */
  private def mkPFun(fun:PrimFun):FunEntry = fun.name match {
    case "drain" =>
      FunEntry(TFun(TTensor(TVar(freshVar()),TVar(freshVar())),TUnit), Context())
    case "nowriter" =>
      FunEntry(TFun(TUnit, TVar(freshVar())), Context())
    case "noreader" =>
      FunEntry(TFun(TVar(freshVar()),TUnit), Context())
    case _ =>
      val tVar = TVar(freshVar())
      // only fifofull has data params and has same type IO
      val dataType = fun.params.map(_=>tVar)
      val inSize = fun.sb._1.inputs.size
      val outSize = fun.sb._1.outputs.size
      val inType = List.fill(inSize)(tVar).foldRight[TExp](TUnit)(TTensor)
      val outType = List.fill(outSize)(tVar).foldRight[TExp](TUnit)(TTensor)
      FunEntry(TFun(Simplify(inType),Simplify(outType)), Context(), dataType)
  }


  /**
    * Infer the type of a block
    * @param block the block
    * @param ctx a context
    * @return an updated context,
    *         the type of the block,
    *         a set of unresolved type constraints, and
    *         a typed block
    */
  private def infer(block:Block,ctx:Context):BTypeResult = block match  {
    case Nil => (ctx,TUnit,Set(),List())
    case b::bs =>
      val (bCtx,bT,bTCons,bTb) = infer(b,ctx)
      val (bsCtx,bsT,bsTCons,bsTb) = infer(bs,bCtx)
      (bsCtx,TTensor(bT,bsT),bTCons++bsTCons,bTb::bsTb)
  }

  private def infer(st:Statement,ctx:Context):STypeResult = st match {
    case f@FunctionApp(Match,_) =>
      throw new TypeException(s"Cannot infer the type of 'match' in ${f.pos}. " +
        s"Try assigning its output to a sequence of variables")
    case se:StreamExpr => infer(se,ctx)
    case a@Assignment(variables, expr) =>
      val (ectx,tcons,lhsTypes,etse) = inferAsg(variables.map(_.x),expr,ctx)
      (ectx,TUnit,tcons,TAssignment(a,lhsTypes,etse))
    case a@RAssignment(variables, expr) =>
      val (ectx,tcons,lhsTypes,etse) = inferAsg(variables.map(_.x),expr,ctx)
      (ectx,TUnit,tcons,TRAssignment(a,lhsTypes,etse))
    case fd@FunDef(name, params, typ, block) =>
      val insNames = params.map(i=> i.name).toSet
      if (insNames.size != params.size)
        new TypeException(s"Cannot repeat input variables on a function definition ${insNames.mkString(",")} found")
      //create a new context for the function that knows only function names and type names - no variable names
//      var fctx = Context(ctx.adts,ctx.functions,Map(),Map())
      var fctx = Context.newScope(ctx)
      // create a type for each input port (specified or new type variable)
      val insPorts:List[(String,TExp)] = params.map(p=> (p.name, getSpecifiedType(p.typ,ctx)))
      // create a type for the function (specified or new type variable)
      val specifiedFType = getSpecifiedType(typ,ctx)
      // create a fresh type variable for each specified ports whose specified type is a type variable
      // plus, if the function has a specified type, use it it as well
//      val substParams = Substitution((insPorts.flatMap(p=> p._2.vars)++specifiedFType.vars)
//        .map(v => v->TVar(freshVar())).toMap)
      val substParams = freshTypes(specifiedFType::insPorts.map(_._2))
      val freshInsPorts = insPorts.map(p=>(p._1,substParams(p._2)))
      // add to the context the data params (NOT FOR NOW) and input params
      val insTypes:List[TExp] = freshInsPorts.map(p => {fctx = fctx.add(p._1,PortEntry(In,p._2)); p._2})
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
      val inOutTCons = matchPorts(bctx.ports)
      // unify constraints from body ++ ports constrains ++ constraint that tfun must match specified fun type
      val substitution = TypeCheck.solve(btcons++inOutTCons++Set(TCons(tfun.tOut,specifiedFType)),ctx)
      val subsTFun = TFun(Simplify(substitution(tfun.tIn)),Simplify(substitution(tfun.tOut)))
      // create a function entry
      val funEntry = FunEntry(subsTFun, substitution(fctx)) //todo: update if we eventually have recursion
      (ctx.add(name,funEntry),TUnit,Set(),TFunDef(fd,funEntry.tExp,substitution(btb,bctx)))//Set())//btcons++inOutTCons)
//    case FunDef(name, params, typ, block)  => // already defined
//      throw new RuntimeException(s"Name $name already defined in the context")
    case sb@SBDef(name, mem, params, init, gcs,outs)  if !ctx.hasFun(name) =>
      val insNames = params.map(i=> i.name).toSet
      if (insNames.size != params.size)
        new TypeException(s"Cannot repeat input variables on a function definition ${insNames.mkString(",")} found")
      //create a new context for the function that knows only function names and type names - no variable names
      var fctx = Context.newScope(ctx) //ctx.adts,ctx.functions,Map(),Map())
      // create a type for each input port (specified or new type variable)
      val insPorts:List[(String,TExp)] = params.map(p=> (p.name, getSpecifiedType(p.typ,ctx)))
      val memsType:List[(String,TExp)] = mem.map(m=>(m.name,getSpecifiedType(m.typ,ctx)))
      // create a type for the function (specified or new type variable)
      val specifiedFType:TExp = TVar(freshVar())
      // create a fresh type variable for each specified ports whose specified type is a type variable
      // plus, if the function has a specified type, use it it as well
//      val substParams = Substitution((insPorts.flatMap(p=> p._2.vars)
//        ++memsType.flatMap(_._2.vars)
//        ++specifiedFType.vars)
//        .map(v => v->TVar(freshVar())).toMap)
      val substParams = freshTypes(specifiedFType::(insPorts++memsType).map(_._2))
      val freshInsPorts = insPorts.map(p=>(p._1,substParams(p._2)))
      val freshMems = memsType.map(m=>(m._1,substParams(m._2)))
      freshMems.foreach(m=>{fctx=fctx.add(m._1,VarEntry(m._2))})
      // add to the context the data params (NOT FOR NOW) and input params
      val insTypes:List[TExp] = freshInsPorts.map(p => {fctx = fctx.add(p._1,PortEntry(In,p._2)); p._2})
      // get the type of the body
      val (gcctx,gccons) = inferGCs(gcs.toList,mem.map(_.name),fctx)
      val touts = outs.map(o=> o->TVar(freshVar()))
      var nctx = gcctx
      for ((o,to) <- touts){
        nctx = nctx.add(o,PortEntry(In,to))
      }
      val (ictx,icons) = inferCmds(init.toSet,mem.map(_.name),nctx)
      nctx = ictx
      val initVars = init.map(_.variable)
      if ((initVars.toSet--mem.map(_.name).toSet).nonEmpty)
        throw new TypeException("Only memory variables can be use in initial commands")
      if (init.flatMap(_.term.vars).nonEmpty)
        throw new TypeException("Only ground terms can be use on the rhs of an initial command")
      val tout = Simplify(touts.map(_._2).foldRight[TExp](TUnit)(TTensor))
      // create the function type
      var tfun = TFun(Simplify(insTypes.foldRight[TExp](TUnit)(TTensor)),tout)
      //// create a function entry
      //val funEntry = FunEntry(tfun,fctx) //todo: update if we eventually have recursion
      // check the ports context is closed
      TypeCheck.isClosed(sb,nctx.ports.filterNot(p=> insNames.contains(p._1)))
      // match input/output context
      val inOutTCons = matchPorts(nctx.ports)
      // unify constraints from body ++ ports constrains ++ constraint that tfun must match specified fun type
      val substitution = TypeCheck.solve(gccons++icons++inOutTCons++Set(TCons(tfun.tOut,specifiedFType)),ctx)
      val subsTFun = TFun(Simplify(substitution(tfun.tIn)),Simplify(substitution(tfun.tOut)))
      // create a function entry
      val funEntry = FunEntry(subsTFun, substitution(fctx)) //todo: update if we eventually have recursion
      (ctx.add(name,funEntry),TUnit,Set(),TSBDef(sb,funEntry.tExp))//Set())//btcons++inOutTCons)
    case SBDef(name, _, _, _, _, _) =>
      throw new RuntimeException(s"Name $name already defined in the context")
////    TODO: case SFunDef(name, typ, sfun) =>
////      _
  }

  private def inferGCs(gcs:List[GuardedCommand],mem:List[String],ctx:Context):(Context,Set[TCons]) = gcs match {
    case Nil => (ctx,Set())
    case gc::more =>
      val (gcctx,gccons) = inferGC(gc,mem,ctx)
      val (morectx,morecons) = inferGCs(more,mem,gcctx)
      (morectx,morecons++gccons)
    }

  private def inferGC(gc:GuardedCommand,mems:List[String],ctx:Context):(Context,Set[TCons]) = {
    val (gctx,gtcons) = inferGuards(gc.guard,mems,ctx)
    val (cctx,ctcons) = inferCmds(gc.cmd,mems,gctx)
    val cmdIns = gc.cmd.flatMap(c=>c.term.vars)
    if ((cmdIns -- gc.inputs ).nonEmpty)
      throw new TypeException("Input ports in a command must be gotten in a guard")
    if ((gc.outputs intersect (gc.inputs--mems)).nonEmpty)
      throw new TypeException("Input ports can not be assigned in a command")
    (cctx,gtcons++ctcons)
  }

  private def inferGuards(g:Guard,mem:List[String],ctx:Context) = {
    var nctx = ctx
    var tcons:Set[TCons] = Set()
    val inputs = g.variables -- mem
    val isQs = g.guards.collect({case gi:IsQ => gi})
    val isQinputs = isQs.flatMap(gi => gi.arg.vars)

    if ((isQinputs -- inputs -- mem).nonEmpty)
      throw new TypeException("Variables inside an IsQ guard must be gotten")

    var tvar = TVar(freshVar())
    inputs.foreach(i => {nctx = nctx.add(i,PortEntry(In,tvar))})

    for (m <- mem) {
      if (nctx.hasVar(m))
        tcons += TCons(nctx.getVar(m).tExp,tvar)
      else
        nctx = nctx.add(m,VarEntry(tvar))
    }
    var tisQs:List[(Context,TExp,Set[TCons])] = List()
    for (isq <- isQs) {
      val tisq = inferIsQ(isq, mem,nctx)
      tisQs :+= tisq
      nctx = tisq._1
    }
    (nctx,tcons++tisQs.flatMap(_._3))
  }

  private def inferIsQ(g:IsQ,mem:List[String],ctx:Context):(Context,TExp,Set[TCons]) =
    if (ctx.constructors.contains(g.q)) {
      val (tctx, tt, ttcons) = inferTerm(g.arg, mem, ctx)
      val qentry = ctx.constructors(g.q)
//      val subst = Substitution((qentry.paramsType.flatMap(_.vars) ++ qentry.tExp.vars)
//        .map(v => v -> TVar(freshVar())).toMap)
      val subst = freshTypes(qentry.tExp::qentry.paramsType)
      val tq = subst(qentry.tExp)
      (tctx, tq, ttcons + TCons(tt, tq))
    } else throw new UndefinedNameException(s"Undefined Constructor name $g.q")

  private def inferCmds(cmds:Set[Command],mem:List[String],ctx:Context) = {
    var nctx = ctx
    var tcons:Set[TCons] = Set()
    for (cmd <- cmds) {
     val (cctx,ctcons) = inferCmd(cmd,mem,nctx)
     nctx = cctx
     tcons ++= ctcons}
    (nctx,tcons)
  }

  private def inferCmd(cmd:Command,mem:List[String],ctx:Context):(Context,Set[TCons]) = {
    val (tctx,tt,ttcons) = inferTerm(cmd.term,mem,ctx)
    var tvar:TExp = TUnit
    var nctx = tctx
    if (mem.contains(cmd.variable)) {
      if (ctx.hasVar(cmd.variable))
        tvar = ctx.getVar(cmd.variable).tExp
      else {
        tvar = TVar(freshVar())
        nctx = nctx.add(cmd.variable,VarEntry(tvar))
      }
    } else {
      tvar = TVar(freshVar())
      nctx = nctx.add(cmd.variable,PortEntry(Out,tvar))
    }
    (nctx,Set(TCons(tvar,tt)))
  }

  private def inferTerm(term:Term,mem:List[String], ctx:Context):(Context,TExp,Set[TCons]) = term match {
    case Var(name) if mem.contains(name) && ctx.hasVar(name) =>
      (ctx,ctx.getVar(name).tExp,Set())
    case Var(name) if mem.contains(name) =>
      val t = TVar(freshVar())
      (ctx.add(name,VarEntry(t)),t,Set())
    case Var(name) =>
      val (vctx,vt,vtcons,vtp) = infer(Port(name),ctx)
      (vctx,vt,vtcons)
    case Q(name,args) if ctx.constructors.contains(name) =>
      val qentry:ConstEntry = ctx.constructors(name)
      TypeCheck.numParams(args.size,qentry.paramsType.size,"")
      var nctx = ctx
      var targs:List[(Context,TExp,Set[TCons])] = List()
      for (a <- args) {
        val targ = inferTerm(a,mem,nctx)
        targs:+=targ
        nctx = targ._1}
//      val subst = Substitution((qentry.paramsType.flatMap(_.vars)++qentry.tExp.vars)
//        .map(v => v->TVar(freshVar())).toMap)
      val subst = freshTypes(qentry.tExp::qentry.paramsType)
      val tq = subst(qentry.tExp)
      val fpType = qentry.paramsType.map(subst(_))
      val tcons = targs.map(t => t._2).zip(fpType).map(p => TCons(p._1,p._2))
      (nctx,tq,targs.flatMap(t=>t._3).toSet++tcons)
    case Q(name,_) => throw new UndefinedNameException(s"Undefined Constructor name $name")
    case GetQ(name,idx,term) if ctx.constructors.contains(name) =>
      val qentry:ConstEntry = ctx.constructors(name)
//      val subst = Substitution((qentry.paramsType.flatMap(_.vars)++qentry.tExp.vars)
//        .map(v => v->TVar(freshVar())).toMap)
      val subst = freshTypes(qentry.tExp::qentry.paramsType)
      val tq = subst(qentry.tExp)
      val fpType = qentry.paramsType.map(subst(_))
      val (tctx,tt,ttcons) = inferTerm(term,mem,ctx)
      val psize = if (fpType.size==0) 1 else fpType.size
      println(psize + "Sixe")
      if (idx<1 || idx > psize)
        throw new IndexOutOfBoundsException(s"Constructor $name has ${psize} parameters")
      (tctx,if (fpType.size==0) TBase("Unit",List()) else fpType.apply(idx),Set(TCons(tq,tt)))
    case GetQ(name,_,_) => throw new UndefinedNameException(s"Undefined Constructor name $name")
  }

  private def inferAsg(variables:List[String],expr:StreamExpr,ctx:Context):(Context,Set[TCons],List[TExp],TStreamExpr) = {
    //check that each variable on the lhs, if it exists, is a variable (VAR) and names don't repeat
    TypeCheck.lhsAssigAreVars(variables,ctx)
    // new context
    var nctx = ctx
    // creat a fresh variable for each lhs variable
    val lhsTypes = variables.map(v=> TVar(freshVar()))
    // add each variable as an Output variable
    variables.zip(lhsTypes).foreach(v => nctx = nctx.add(v._1,PortEntry(Out,v._2)))
    // get the type of the expression
    val (ectx,et,etcons,ete) = infer(expr,nctx)
    // check num params match (if not destructor)
    if (!et.isInstanceOf[TDestr]) {
      TypeCheck.numParams(numOutputs(et), variables.size, variables.mkString(",")+" <- "+Show(expr))
    }
    // create a tensor type for the lhs variables
    val lhsTTensor = Simplify(lhsTypes.foldRight[TExp](TUnit)(TTensor))
    // create a type constraint
    val tcons = TCons(lhsTTensor,et)
    (ectx,etcons+tcons,lhsTypes,ete)
  }

  private def getSpecifiedType(tn:MaybeTypeName, ctx:Context):TExp = tn match {
    case Some(t) =>
      val specified = mkType(t)
       if (existsType(specified,ctx))
        if (matched(specified,ctx.getType(t.name).tExp))
          specified
        else
          throw new InvalidType(s"Unknown type ${Show(t)}")
      else
        throw new UndefinedNameException(s"Unknown type name ${t.name}")
    case _ => TVar(freshVar())
  }

  private def matched(te:TExp, tdef:TExp):Boolean =
    try {
      Unify(Set(TCons(te,tdef)))
      true
    } catch {
      case e:TypeException => false
    }

  private def existsType(te:TExp,ctx:Context):Boolean = te match {
    case TBase(name,ps) if ctx.hasType(name) && ps.forall(p=>existsType(p,ctx)) => true
    case TBase(name,_) => throw new UndefinedNameException(s"Unknown type name ${name}")
    case TTensor(t1,t2) => existsType(t1,ctx) && existsType(t2,ctx)
    case TFun(ins,outs) => existsType(ins,ctx) && existsType(outs,ctx)
    case _ => true
  }

  private def requires(prop:Boolean,msg:String): Unit =
    if (!prop) throw new TypeException(msg)

  private def infer(gt:GroundTerm,ctx:Context):GTTypeResult = gt match {
    case Port(x) =>
      // create a new type variable
      val pt = TVar(freshVar())
      // add it to the context as a new Input port
      (ctx.add(x,PortEntry(In,pt).setPos(gt.pos)),pt,Set(),TPort(x,pt))
    case const@Const(q,args) if ctx.hasConstructor(q)  =>
      // get constructor entry
      val qentry:ConstEntry = ctx.getConst(q)
      //check the number of expected parameters match
      //TypeCheck.numParams(args.size,qentry.paramsType.size)
      requires(args.size == qentry.paramsType.size,
        s"[${const.pos}]: Number of formal and actual parameters don't match in ${const.pos}")
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
      //val subst = Substitution((qentry.paramsType.flatMap(_.vars)++qentry.tExp.vars)
      //  .map(v => v->TVar(freshVar())).toMap)
      val subst = freshTypes(qentry.tExp::qentry.paramsType)
      val qFType = subst(qentry.tExp)
      // fresh formal params
      val fpType = qentry.paramsType.map(subst(_))
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
      val (sfctx,sft,sfcons,tsf):SFTypeResult = infer(sfun,ctx)
      //println("Function ctx: "+ sfctx)
      val sfType:TFun = TypeCheck.isFunType(sft)
      //check the number of expected parameters match (if expected type is not destructor - build)
      if (!sfType.tIn.isInstanceOf[TDestr]) {
        TypeCheck.numParams(args.size, numInputParams(sfType), Show(se))
      }
      // get the type of each actual param
      var nctx = sfctx
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
      (nctx,sfType.tOut,apType.flatMap(r=> r._3).toSet++tcons++sfcons,TFunApp(tsf,sfType.tOut,apType.map(_._4)))
  }

  private def infer(sf:StreamFun,ctx:Context):SFTypeResult = sf match {
    case FunName(f,d) if ctx.functions.contains(f) =>
      // get function entry
      val fEntry = ctx.functions(f)
      // get fresh type for the function, including its data params
      //val subst = Substitution(fEntry.tExp.vars.map(v => v->TVar(freshVar())).toMap ++
      //  fEntry.dataParamsType.flatMap(te=>te.vars).map(v=>v->TVar(freshVar())))
      val subst = freshTypes(fEntry.tExp::fEntry.dataParamsType)
      // substitute types accordingly
      val fFType = subst(fEntry.tExp)
      val dFtype = fEntry.dataParamsType.map(p => subst(p))
      // infer the types of data params
      var nctx = ctx
      var dtype:List[GTTypeResult] = List()
      for (a <- d) {
        val aType = infer(a,nctx)
        dtype :+= aType
        nctx = aType._1
      }
      // tensor for data args type
      val dataArgsType = Simplify(dtype.map(r => r._2).foldRight[TExp](TUnit)(TTensor))
      //println(s"[FUN NAME]\nData Arguments Types:\n ${Show(dataArgsType)}")
      // tensor for data params type
      val dataPsType = Simplify(dFtype.foldRight[TExp](TUnit)(TTensor))
      //println(s"[FUN NAME]\nData Params Types:\n ${dFtype.mkString(",")}\n ${Show(dataPsType)}")
      // get type constraints from actual to formal params
      val dcons:Set[TCons] = if (d.isEmpty) Set() else Set(TCons(dataPsType,dataArgsType))
      //println(s"[FUN NAME]\nConstraints: ${dcons.mkString(",")}")
      (ctx,fFType,dcons,TFunName(f,fFType,dtype.map(r => r._4)))
    case FunName(f,d) => //TODO: Check if it makes sense
      // special case, if it doesn't exists assume it has to be a 1->1 sync with a name
      val tVar = TVar(freshVar())
      // create dummy function type
      val ftype = TFun(tVar,tVar)
      // add dummy function to the context?
      val funEntry = FunEntry(ftype, Context.newScope(ctx))//Context(ctx.adts,ctx.functions,Map(),Map()))
      (ctx.add(f,funEntry),ftype,Set(),TFunName(f,ftype))
    case SeqFun(f1,f2) =>
      val (f1ctx,f1t,f1tcons,sft1) = infer(f1,ctx)
      val (f2ctx,f2t,f2tcons,sft2) = infer(f2,f1ctx)
      val tf1:TFun = TypeCheck.isFunType(f1t)
      val tf2:TFun = TypeCheck.isFunType(f2t)
      // check number outputs from f1 match number of inputs from f2 (only if not destructor)
      if (!tf1.tOut.isInstanceOf[TDestr] && !tf2.tIn.isInstanceOf[TDestr]) {
        TypeCheck.numParams(numOutputs(tf1.tOut),numInputParams(tf2), Show(sf))
      }
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

  private def numInputParams(f:TFun):Int = {
    def numParams(te:TExp):Int = te match {
      case TUnit => 0
      case TBase(_,_) | TVar(_) => 1
      case TTensor(t1,t2) => numParams(t1) + numParams(t2)
      case _ => 0
    }
    numParams(f.tIn)
  }

  private def numOutputs(te:TExp):Int = te match {
    case TUnit => 0
    case TBase(_,_) | TVar(_) => 1
    case TTensor(t1,t2) => numOutputs(t1) + numOutputs(t2)
    case TFun(_,tout) => numOutputs(tout)
    case _ => 0
  }

  /**
    * Given a list of type expressions find a substitution with fresh type variables
    * @param types a list of type expressions
    * @return a substitution mapping type variables in types to fresh type variables
    */
  private def freshTypes(types:List[TExp]):Substitution = {
    val typeVariables = types.flatMap(t=>t.vars)
    Substitution(typeVariables.map(v=>v->TVar(freshVar())).toMap)
  }

}
