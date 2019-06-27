package dsl.analysis.semantics

import dsl.DSL
import dsl.analysis.syntax.{ConnDef, TypeDecl, Variant}
import dsl.backend.Prettify
import dsl.common.TypeException
import preo.ast.CPrim
import preo.backend.Network
import preo.backend.Network.Prim

/**
  * Created by guillecledou on 2019-06-19
  */


/**
  * // todo: probably it can be renamed and act as general function type
  * Connector type consisting of the type of its input and output parameters
  * @param ins type of each input parameter
  * @param outs type of each output parameter
  */
case class TypeConn(ins:List[TypeExpr],outs:List[TypeExpr]) {

  def getType:TypeExpr = {
    if (ins.isEmpty && outs.isEmpty)
      TUnit
    else
      TMap(this.getInputType,this.getOutputType)
  }

  def getOutputType:TypeExpr =
    if (outs.size>1)
      TProd(outs.head,outs.tail)
    else {
      if (outs.isEmpty) TUnit else outs.head
    }


  def getInputType:TypeExpr =
    if (ins.size>1)
      ins.init.foldRight[TypeExpr](ins.last)(TMap(_,_))
    else {
      if (ins.isEmpty) TUnit else ins.head
    }

}

/**
  * Helper to type a connector definition
  */
object TypeConn {

  private var tVars:Int = 0
  private def freshVar():String = {tVars+=1; s"${tVars-1}"}

  /**
    * Type a connector definition
    * todo: can be done more efficiently without so many constraints
    * @param connDef connector definition
    * @return the type expression of the defined connector
    */
  def apply(connDef: ConnDef):TypeConn = {
    // initialize seeds
    tVars =0
    // reset prettifier
    Prettify.reset()
    // convert the defined connector into a network
    var network = Network(DSL.unsafeCoreConnector(connDef.c))
    // infer connector type
    val (tvar,const) = infer(connDef)//network
    // try to unify and substitute
    val substitutions:Map[TVar,TypeExpr] = Substitute(Unify(const))
    // base on the substitutions, replace variables by the corresponding typeExpr in inputs and outputs types,
    // and prettify free variables
    var ins =
      network.ins.map(i=> tvar(i)).map(tv=> Prettify(tv.substitute(tv,substitutions.getOrElse(tv,tv))))
    var outs =
      network.outs.map(i=> tvar(i)).map(tv=> Prettify(tv.substitute(tv,substitutions.getOrElse(tv,tv))))
    // return the type of the connector definition
    TypeConn(ins,outs)
  }


  /**
    * Given a connector definition, returns its type
    * todo (if connector definition can refer to other connectors def or identifiers ):
    *   - add support to receive a context of identifiers and their types, and
    *   - add support to receive a context of known connectors with their types
    * @param connDef
    * @param adt
    * @return
    */
  private def infer(connDef:ConnDef):(Map[Int,TVar],Set[TCons]) = { //:(TVar,Set[TCons]) = {
    def mkTCons(tvars:List[TypeExpr]):Set[TCons] = tvars match {
      case Nil => Set()
      case h::Nil=> Set()
      case h::tail => mkTCons(tail)+TCons(h,tail.head)
    }
    // convert the defined connector into a network
    var network = Network(DSL.unsafeCoreConnector(connDef.c))
    // check if the network is closed or doesn't have outputs
    (network.ins,network.outs) match {
      // closed network
      case (Nil,Nil) =>
        var T = TVar(freshVar)
         (Map(),Set())
//        (T,Set(TCons(T,TUnit)))
      // no outputs
      case (ins,Nil) =>
        // for each in create a new freshvar
//        var Tins = ins.map(i=>TVar(freshVar()))
        // the type of the connector is Tin1 -> ... -> TinN -> TUnit
//        var TConn = Tins.foldRight[TypeExpr](TUnit)(TMap(_,_))
//        var T = TVar(freshVar())
         (ins.map(i => i->TVar(freshVar())).toMap,Set())
//        (T,Set(TCons(T,TConn)))
      // possible inputs and definitely outputs
      case (ins,outs) =>
        // infer the type for each prim
        var Tprims = network.prims.flatMap(p=> infer(p))
        // group types by port
        var portTypes = Tprims.groupBy(_._1).mapValues(_.map(_._2))
        // for each port that has more that one type variable, make a constraint for them to be equal
        var newCons = portTypes.flatMap(pt=> mkTCons(pt._2)).toSet
        // make the type of the connector: Tin1 -> ... -> TinN -> Tout1 x ... x ToutN
        // first, the type of the output
        var Tout =  if (outs.size>1) TProd(portTypes(outs.head).head,outs.tail.map(portTypes(_).head))
                    else portTypes(outs.head).head
        var TConn = (ins).map(p=> portTypes(p).head)
          .foldRight[TypeExpr](Tout)(TMap(_,_))
        var T = TVar(freshVar())
        var Tports = (ins++outs).map(p => p->TVar(freshVar())).toMap//((ins++outs).map(p=> portTypes(p).head))
        (Tports,newCons++Tports.map(e=> TCons(e._2,portTypes(e._1).head)))
//        (T,newCons+TCons(T,TConn))
    }
  }

  /**
    * Get the type of a primitive connector
    *
    * @param prim
    * @return a map from each port to its type
    */
  private def infer(prim:Prim):Map[Int,TypeExpr] = prim match {
    case Prim(CPrim(name,_,_,_),List(a),List(b),_) if name.matches("(sync|fifo|id)")=>
      val T = TVar(freshVar())
      Map(a->T,b->T)
    // todo: the type of a lossy should be a -> a OR a -> Opt[a] OR some other special type?
    case Prim(CPrim("lossy",_,_,_),List(a),List(b),_) =>
      val T = TVar(freshVar())
      Map(a->T,b->TOpt(T))
    case Prim(CPrim("drain",_,_,_),List(a,b),List(),_) =>
      Map(a->TVar(freshVar()),b->TVar(freshVar()))
    case Prim(CPrim("fifofull",_,_,_),List(a),List(b),_) =>
      // special case, todo: add support to declare the type of the fifofull when using it
      // for now assume unspecified
      val T = TVar(freshVar())
      Map(a->T,b->T)
    case Prim(CPrim("merger",_,_,_),List(a,b),List(c),_) =>
      val Ta = TVar(freshVar())
      val Tb = TVar(freshVar())
      Map(a->Ta,b->Tb,c->TEithers(Ta,List(Tb)))
    case Prim(CPrim("dupl",_,_,_),List(a),List(b,c),_) =>
      val T = TVar(freshVar())
      Map(a->T,b->T,c->T)
    case Prim(CPrim("writer",_,_,_),List(),List(a),_) =>
      // special case, todo: add support to declare what the writer writes
      // for now assume unspecified
      Map(a->TVar(freshVar()))
    case Prim(CPrim("reader",_,_,_),List(a),List(),_) =>
      Map(a->TVar(freshVar()))
    // a node that is only a duplicator, i.e. has one input only
    case Prim(CPrim("node",_,_,extra), List(a), outs, _) if extra.intersect(Set("dupl")).nonEmpty =>
      val Ta = TVar(freshVar())
      val Touts = outs.map(o=> o->Ta).toMap
      Touts+(a->Ta)
    // a node that is a merger and possible a duplicator, i.e. has more than one input
    case Prim(CPrim("node",_,_,extra), ins, outs, _) if extra.intersect(Set("dup","mrg")).nonEmpty =>
      val varsIns = ins.map(i => TVar(freshVar()))
      val Tout = TEithers(varsIns.head, varsIns.tail)
      ins.zip(varsIns).toMap++outs.map(o=> o->Tout).toMap
    // todo: a XOR types should be a -> a x a  OR  a -> Opt[a] x Opt[a] or some other special type?
    case Prim(CPrim("node",_,_,extra), ins, outs, _) if extra.intersect(Set("xor")).nonEmpty =>
      val varsIns = ins.map(i => TVar(freshVar()))
      // val Tout = TOpt(TEithers(varsIns.head, varsIns.tail))
      val Tout = if (ins.size>1) TEithers(varsIns.head, varsIns.tail) else varsIns.head
      ins.zip(varsIns).toMap++outs.map(o=> o->Tout)
    // any unknown prim 1->1
    case Prim(CPrim(name,_,_,_),List(a),List(b),_) =>
      val T = TVar(freshVar())
      Map(a->T,b->T)
    case Prim(p, ins,outs,_) =>
      throw new TypeException(s"Unknown type for primitive ${p.name}")
  }
}
