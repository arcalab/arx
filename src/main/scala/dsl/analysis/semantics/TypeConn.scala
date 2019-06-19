package dsl.analysis.semantics

import dsl.DSL
import dsl.analysis.syntax.{ConnDef, TypeDecl, Variant}
import dsl.common.TypeException
import preo.ast.CPrim
import preo.backend.Network
import preo.backend.Network.Prim

/**
  * Created by guillecledou on 2019-06-19
  */


/**
  * Helper to type a connector definition
  */
object TypeConn {

  private var tVars:Int = 0
  private def freshVar():String = {tVars+=1; s"_${(tVars-1)}"}
  private var prettiSeed = 1
  private def prettifySeed():Int = {prettiSeed+=1; prettiSeed-1}

  /**
    * Type a connector definition
    * todo: can be done more efficiently without so many constraints
    * @param connDef connector definition
    * @return the type expression of the defined connector
    */
  def apply(connDef: ConnDef):TypeExpr = {
    // initialize seeds
    tVars =0
    prettiSeed = 0
    // infer connector type
    val (tvar,const) = infer(connDef)
    // try to unify and substitute
    val substitutions:Map[TVar,TypeExpr] = Substitution(Unify(const))
    prettify(tvar.substitute(tvar,substitutions(tvar)))
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
  private def infer(connDef:ConnDef):(TVar,Set[TCons]) = {
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
        (T,Set(TCons(T,TUnit)))
      // no outputs
      case (ins,Nil) =>
        // for each in create a new freshvar
        var Tins = ins.map(i=>TVar(freshVar()))
        // the type of the connector is Tin1 -> ... -> TinN -> TUnit
        var TConn = Tins.foldRight[TypeExpr](TUnit)(TMap(_,_))
        var T = TVar(freshVar())
        (T,Set(TCons(T,TConn)))
      // possible inputs and definitely outputs
      case (ins,outs) =>
        // infer the type for each prim
        var Tprims = network.prims.flatMap(p=> infer(p))
        // group types by port
        var portTypes = Tprims.groupBy(_._1).mapValues(_.map(_._2))
        // make for each port that has more that one type variable, make a constraint for them to be equal
        var newCons = portTypes.flatMap(pt=> mkTCons(pt._2)).toSet
        // make the type of the connector: Tin1 -> ... -> TinN -> Tout1 x ... x ToutN
        // first, the type of the output
        var Tout =  if (outs.size>1) TProd(portTypes(outs.head).head,outs.tail.map(portTypes(_).head))
                    else portTypes(outs.head).head
        var TConn = (ins).map(p=> portTypes(p).head)
          .foldRight[TypeExpr](Tout)(TMap(_,_))
        var T = TVar(freshVar())
        (T,newCons+TCons(T,TConn))
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

  private def prettify(te: TypeExpr):TypeExpr = {
    var known:Map[String,String] = Map()
    def pretty(te: TypeExpr): TypeExpr = te match {
      case t@TVar(n) =>
        if (n.startsWith("_")) {
          if (known.contains(n))
            TVar(known(n))
          else {
            var s = intToAlph(prettifySeed())
            known+= (n->s)
            TVar(s)
          }
        } else t
      case TMap(t1, t2) => TMap(pretty(t1), pretty(t2))
      case TOpt(t) => TOpt(pretty(t))
      case TProd(t, ts) => TProd(pretty(t), ts.map(pretty))
      case TEithers(t, ts) => TEithers(pretty(t), ts.map(pretty))
      case TTuple(t, ts) => TTuple(pretty(t), ts.map(pretty))
      case t => t
    }
    pretty(te)
  }

  private def intToAlph(i:Int):String = {
    var quotient = i/26
    var rem = i%26
    var res = ('a'.toInt + rem).toChar
    if (quotient == 0 ) res.toString else intToAlph(quotient-1)+res
  }

}
