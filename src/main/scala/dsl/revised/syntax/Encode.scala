package dsl.revised.syntax

import dsl.revised.core.{Automaton, Connector, Network, Rule}
import Program.*
import dsl.revised.{Error, Prelude}
import dsl.revised.core.Connector.{CAut, CNet}
import dsl.revised.core.Network.{Constructor, Link}
import dsl.revised.core.Term.vars

object Encode:
  /** Encode a syntactic program into a core network, removing syntactic sugar and simplifications. */
  def apply(p:Program, imported:Set[String]=Set()): Network =
    apply(collectDecl(p.main,p.modules+("reo"->Prelude.reoModule),Set()),Network.empty)

  /** Traverse imports to collect all needed declarations. */
  def collectDecl(module: Module, scope:Map[String,Module], done:Set[String]): List[Decl] = module match
    case Module(Nil,decls) => decls
    case Module(nxt::rest,decls) =>
      if done contains nxt then
        collectDecl(Module(rest,decls),scope,done)
      else if scope contains nxt then
        collectDecl(scope(nxt),scope,done+nxt) ::: collectDecl(Module(rest,decls),scope,done+nxt)
      else
        Error.invalid(s"Module $nxt not found.")


//  private val defNet = Network(Map(),dsl.revised.core.Term.preludeInterpretations,Map(),Nil)

  def apply(decl:List[Decl],net:Network): Network = decl match
    case Nil => net
    case d::rest => apply(rest,apply(d,net,0)._1)

//  def apply(decl:Decl,net:Network): Network = Error.invalid("not implemented yet")

  def apply(decl:Decl,net:Network,seed:Int): (Network,Int) = decl match
    case d:DataDecl => (apply(d,net),seed)
    case a:AutDecl => (apply(a,net),seed)
    case n:NetDecl => (apply(n,net),seed)
    case l:LinkDecl => apply(l,net,seed)
//    case r:ReturnDecl => apply(r,net)

  def apply(decl:DataDecl,net:Network): Network =
    if net.data contains decl.name then
      Error.invalid(s"data type ${decl} is already defined.")
    else
      // no need to add function interpretations, and function types will come during type inference
      Network(net.data + (decl.name -> (decl.args,decl.const)), net.functions, net.connectors, net.links)

  def apply(decl:AutDecl,net:Network): Network =
    if net.connectors contains decl.name then
      Error.invalid(s"Automaton ${decl} is already declared.")

    val args = decl.args.toSet
    val newAut = expandAut(decl.aut,args,decl.inputs,decl.outputs)
    if !newAut.wellDefined then
      Error.invalid(s"Automata not well defined: $newAut")

    val newCAut = CAut(newAut,decl.args,decl.inputs,decl.outputs)
    Network(net.data, net.functions, net.connectors + (decl.name -> newCAut), net.links)

  def apply(decl:NetDecl,net:Network): Network =
    if net.connectors contains decl.name then
      Error.parsing(s"Network ${decl} is already declared.")
    else
      val newNet = CNet(apply(decl.decls,net /*Network.empty*/), decl.args, decl.inputs, decl.outputs )
      Network(net.data, net.functions, net.connectors + (decl.name -> newNet) , net.links)

  def apply(decl:LinkDecl,net:Network, seed:Int): (Network,Int) =
    decl.invoc match
      case PortCall(n) =>
        val newLink = Link("id",Nil,List(n),decl.outputs)
        (Network(net.data, net.functions, net.connectors, newLink::net.links),seed)

      case ConnCall(n,as,ins) =>
        var newSeed = seed
        var newNet = net
        val inTerms = for (call<-ins) yield call match
          case PortCall(n2) => n2
          case c:ConnCall =>
            val newPort = s"a§$newSeed"
            newSeed += 1
            val (x,y) = apply(LinkDecl(c,List(newPort)),newNet,newSeed)
            newNet=x ; newSeed=y
            newPort
        val newLink = Link(n,as,inTerms,decl.outputs)
        newNet = Network(newNet.data,newNet.functions,newNet.connectors,newLink::newNet.links)
        (newNet,newSeed)

  /** Introduce extra "asks" for unused inputs */
  def expandAut(a: Automaton,args:Set[String],inputs:List[String],outputs:List[String]): Automaton =
    val newRs = a.rs.map(expandRule(_,/* a.clocks++ */args++a.clocks))
    val newRegs = a.registers ++ a.init.map(_.v) ++ newRs.flatMap(r => r.upd.map(_.v))
    val newIns  = (a.inputs  ++ newRs.flatMap(r => r.get++r.ask) ++ inputs.toSet) -- newRegs
    val newOuts = a.outputs ++ newRs.flatMap(r => r.assg.map(_.v)) ++ outputs.toSet
    Automaton(a.init,a.inv,newRs,newIns,newOuts,newRegs,a.clocks,a.args++args)

  def expandRule(r:Rule,ok:Set[String]): Rule =
    val readVars = r.pred.flatMap(vars)++r.assg.flatMap(x=>vars(x.t))++r.upd.flatMap(x=>vars(x.t))
    val declared = r.get ++ r.ask ++ ok
//    println(s"expanding $readVars except $declared to ${r.ask}")
    Rule(r.get,r.ask++(readVars--declared),r.ask,r.pred,r.assg,r.upd,r.highlights)