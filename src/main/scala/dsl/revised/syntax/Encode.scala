package dsl.revised.syntax

import dsl.revised.core.{Automaton, Connector, Network, Rule, Term}
import Program._
import Program.Decl._
import Program.InputCall._
import dsl.revised.{Error, Prelude}
import dsl.revised.core.Connector.{CAut, CNet}
import dsl.revised.core.Network.{Constructor, Link}
import dsl.revised.core.Rule.Assignment
import dsl.revised.core.Term.{Interpretation, vars}

object Encode:
  /** Encode a syntactic program into a core network, removing syntactic sugar and simplifications. */
  def apply(p:Program, imported:Set[String]=Set()): Network =
    apply(collectDecl(p.main+(""),p.modules+(""->Prelude.syncModule)+("reo"->Prelude.reoModule),Set()),Network.empty)

  /** Traverse imports to collect all needed declarations. */
  def collectDecl(module: Module, scope:Map[String,Module], done:Set[String]): List[Decl] = module match
    case Module(Nil,decls) => decls
    case Module(nxt::rest,decls) =>
      if done contains nxt then
        collectDecl(Module(rest,decls),scope,done)
      else if scope contains nxt then
        collectDecl(scope(nxt),scope,done+nxt) ::: collectDecl(Module(rest,decls),scope,done+nxt)
      else
        Error.encoding(s"Module $nxt not found.")


//  private val defNet = Network(Map(),dsl.revised.core.Term.preludeInterpretations,Map(),Nil)

  def apply(decl:List[Decl],net:Network): Network = decl match
    case Nil => net
    case d::rest => apply(rest,apply(d,net,0)._1)

//  def apply(decl:Decl,net:Network): Network = Error.invalid("not implemented yet")

  def apply(decl:Decl,net:Network,seed:Int): (Network,Int) =
    decl match
    case d:DataDecl => (apply(d,net),seed)
    case a:AutDecl => (apply(a,net),seed)
    case n:NetDecl => (apply(n,net),seed)
    case l:LinkDecl => apply(l,net,seed)
    case c:ConstDecl => (apply(c,net),seed)
//    case r:ReturnDecl => apply(r,net)

  def apply(c: ConstDecl, net: Network): Network =
    if net.functions contains c.name then
      Error.encoding(s"constant declaration ${c} is already defined (or a function with the same name).")
    else
      Network(net.data, net.functions + (c.name -> ({
        case Nil => c.term
      })), net.connectors, net.links)

  def apply(decl:DataDecl,net:Network): Network =
    if net.data contains decl.name then
      Error.encoding(s"data type ${decl} is already defined.")
    else
      // no need to add function interpretations, and function types will come during type inference
      Network(net.data + (decl.name -> (decl.args,decl.const)), net.functions, net.connectors, net.links)

  def apply(decl:AutDecl,net:Network): Network =
    if net.connectors contains decl.name then
      Error.encoding(s"Automaton ${decl} is already declared.")

    val args = decl.args.toSet
    val newAut = expandAut(decl.aut,args,decl.inputs,decl.outputs,net.functions)
    if !newAut.wellDefined then
      Error.encoding(s"Automata ${decl.name} not well defined. ${newAut.whyBadlyDefined}: $newAut")

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
        val newLink = Link("",Nil,List(n),decl.outputs) // empty link name = Sync/ID
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
        val newLink = Link(n,as.map(replaceConst(net.functions)),inTerms,decl.outputs)
        newNet = Network(newNet.data,newNet.functions,newNet.connectors,newLink::newNet.links)
        (newNet,newSeed)

  /** Introduce extra "asks" for unused inputs */
  def expandAut(a: Automaton,args:Set[String],inputs:List[String],outputs:List[String],fs:Map[String,Interpretation]): Automaton =
    val newFs = fs--(args++a.inputs++a.outputs++a.registers)
    val newRs = a.rs.map(expandRule(_,/* a.clocks++ */args++a.clocks,newFs))
    val newRegs = a.registers ++ (a.init.map(_.v) ++ newRs.flatMap(r => r.upd.map(_.v))) -- a.clocks
    val newIns  = (a.inputs  ++ newRs.flatMap(r => r.get++r.ask) ++ inputs.toSet) -- (newRegs++a.clocks)
    val newOuts = a.outputs ++ (newRs.flatMap(r => r.eqs.map(_.v))--a.clocks) ++ outputs.toSet
    val newInit = a.init.map(replaceConst(newFs))
    val newInv = a.inv.map(replaceConst(newFs))
    Automaton(newInit,newInv,newRs,newIns,newOuts,newRegs,a.clocks,a.args++args)

  def expandRule(r:Rule,ok:Set[String], fs:Map[String,Interpretation]): Rule =
    val readVars = (r.pred.flatMap(vars)++
      r.eqs.flatMap(x=>vars(x.t))++
      r.upd.flatMap(x=>vars(x.t))) --
      fs.keySet
    val declared = r.get ++ r.ask ++ ok
//    println(s"expanding $readVars except $declared to ${r.ask}")
    Rule(r.get,r.ask++(readVars--declared),r.und,r.pred.map(replaceConst(fs)),
      r.eqs.map(replaceConst(fs)),r.upd.map(replaceConst(fs)),r.highlights,r.lbls)

  private def replaceConst(fs: Map[String, Interpretation])(t:Term): Term =
    Term.evaluate(t)(using fs)
    //t // TODO
  private def replaceConst(fs: Map[String, Interpretation])(a:Assignment): Assignment =
    Assignment(a.v,replaceConst(fs)(a.t))
