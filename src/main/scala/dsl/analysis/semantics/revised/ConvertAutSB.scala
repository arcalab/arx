package dsl.analysis.semantics.revised

import dsl.analysis.semantics._
import Rule._

object ConvertAutSB :
  def sb2Automaton(sb: StreamBuilder): Automaton =
    Automaton(sb.init.map(c => Assignment(c.variable, term2revised(c.term))),
      sb.gcs.map(gc2rule(_,sb)), sb.inputs, sb.outputs, sb.memory)

  private def term2revised(term: Term): revised.Term = term match
    case Var(v) => revised.Term.Var(v)
    case Q(n, ts) => revised.Term.Q(n, ts.map(term2revised))
    case GetQ(n, i, t) => revised.Term.getQ(n, i, List(term2revised(t)))

  private def gc2rule(gc: GuardedCommand, sb: StreamBuilder): Rule =
    val get = gc.guard.guards.filter(_.isInstanceOf[Get]).flatMap(_.vars)
    val ask = gc.guard.guards.filter(_.isInstanceOf[Ask]).flatMap(_.vars)
    val und = gc.guard.guards.filter(_.isInstanceOf[Und]).map(_.asInstanceOf[Und].v)
    val pred = gc.guard.guards.filter(_.isInstanceOf[IsQ]).map(x =>
      revised.Term.isQ(x.asInstanceOf[IsQ].q, List(term2revised(x.asInstanceOf[IsQ].arg))))
    val assg = gc.cmd.filter(x => !sb.memory.contains(x.variable)).map(x =>
      Assignment(x.variable, term2revised(x.term)))
    val upd = gc.cmd.filter(x => sb.memory.contains(x.variable)).map(x =>
      Assignment(x.variable, term2revised(x.term)))
    Rule(get, ask, und, pred, assg, upd, gc.highlights)

  def aut2StreamBuilder(a: Automaton): StreamBuilder =
    StreamBuilder(a.init.map(x => Command(x.v, revised2term(x.t))),
      a.rs.map(rule2gc), a.inputs, a.outputs, a.registers)

  private def revised2term(term: revised.Term): Term = term match
    case revised.Term.Var(v) => Var(v)
    case revised.Term.TFun(s, revised.Term.TInt(i) :: List(t))
      if s.startsWith("get§") => GetQ(s.drop(4), i, revised2term(t))
    case revised.Term.TFun(s, ts) if s.startsWith("build§") => Q(s.drop(6), ts.map(revised2term))
    case _ => sys.error(s"[Term2old] unsupported conversion to old terms: $term")

  private def rule2gc(r: Rule): GuardedCommand =
    def term2com(t: revised.Term): IsQ = t match
      case revised.Term.TFun(n, List(t)) if n.startsWith("is§") => IsQ(n.drop(3), revised2term(t))
      case _ => sys.error(s"[rule2gc] predicate is not isQ - not supported: $t")

    val guards: Set[GuardItem] = r.get.map(Get.apply) ++
      r.ask.map(Ask.apply) ++
      r.und.map(Und.apply) ++
      r.pred.map(term2com)
    val cmds = r.assg.map(x => Command(x.v, revised2term(x.t))) ++
      r.upd.map(x => Command(x.v, revised2term(x.t)))
    GuardedCommand(Guard(guards), cmds, r.highlights)