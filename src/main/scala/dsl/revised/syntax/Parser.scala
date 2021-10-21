package dsl.revised.syntax

import cats.parse.{LocationMap, Parser as P, Parser0 as P0}
import cats.parse.Numbers.*
import cats.syntax.all.*
import P.*
import cats.data.NonEmptyList
import cats.parse.Rfc5234.{alpha, sp}
import dsl.revised.core.Network.Link
import dsl.revised.core.Rule.Assignment
import dsl.revised.core.{Automaton, Rule, Term}
import dsl.revised.core.Term.{Fun, IntVal, Var}
import dsl.revised.syntax.Program.{AutDecl, ConnCall, InputCall, LinkDecl, PortCall}

object Parser :

  val whitespace: P[Unit] = P.charIn(" \t\r\n").void
  val sps: P0[Unit] = whitespace.rep0.void
  private[this] val comma = char(',')
  private[this] val scomma = char(';')

  def varName: P[String] =
    (charIn('a' to 'z') ~ alpha.rep0).string
  def funName: P[String] =
    (charIn('A' to 'Z') ~ alpha.rep0).string
  def symbols: P[String] =
    (oneOf("+-><!%/*=".toList.map(char)).rep).string

  /// Terms ///
  def term: P[Term] = P.recursive[Term] { newTerm =>
    def funArgs: P0[List[Term]] =
      (char('(') *> newTerm.surroundedBy(sps).repSep0(comma) <* char(')')).?
        .map(_.getOrElse(Nil))
    def funTerm: P[Term] =
      ((funName <* sps) ~ funArgs).map(Fun.apply)
    def infixTerm: P[Term] = // TODO: missing precedence of symbols - later?
      (simpleTerm ~ symbols.surroundedBy(sps) ~ newTerm).map(x=>Fun(x._1._2,List(x._1._1,x._2)))
    def simpleTerm: P[Term] =
      paren orElse intTerm orElse funTerm orElse varTerm
    def paren: P[Term] =
      char('(') ~ sps *> newTerm <* sps ~ char(')')

    infixTerm.backtrack orElse simpleTerm
  }

  def intTerm: P[Term] =
    digits.surroundedBy(sps).map(i => IntVal(i.toInt))
  def varTerm: P[Term] =
    varName.surroundedBy(sps).map(Var.apply)

  //// RULES ////

//  def rules: P0[List[Rule]] = P.recursive[List[Rule]] { (nxtRule:P[List[Rule]]) =>
//    (rule ~ nxtRule.?).map{
//      case (r,None) => List(r)
//      case (r,rest:List[Rule]) => r::rest
//    }
//  }

  def rule: P[Rule] =
    val arr = string("-->").surroundedBy(sps)
    val rl = guards.with1 ~ arr ~ ruleActs ~ char(';')
    rl.map(x => x._1._1._1 --> x._1._2)

  def varNames: P[Set[String]] =
    ((char('(') *> varName.surroundedBy(sps).repSep(comma)) <* char(')'))
      .map(_.toList.toSet)

  def guards: P0[Rule] = (guard <* sps).repSep0(char(',')~sps)
    .collect(_.foldRight(Rule.empty)(_ & _))
  def guard: P[Rule] = get orElse ask orElse und orElse term.map((t:Term)=>Rule.pred(t))
  def get: P[Rule] = ((string("from")~sps) *> varNames).map(Rule.get)
  def ask: P[Rule] = ((string("at")~sps) *> varNames).map(Rule.ask)
  def und: P[Rule] = ((string("notAt")~sps) *> varNames).map(Rule.und)

  def assgn: P[(String,Term)] = (varName <* (string(":=")).surroundedBy(sps)) ~ term
  def upd:   P[(String,Term)] = (varName <* (char('\'')~sps~string(":=")).surroundedBy(sps)) ~ term

  def ruleActs: P0[Rule] = (ruleAct <* sps).repSep0(char(',')~sps)
    .collect(_.foldRight(Rule.empty)(_ & _))
  def ruleAct: P[Rule] = toR.backtrack orElse assgnR.backtrack orElse updR
  def assgnR: P[Rule] = assgn.map(Rule.assg)
  def updR: P[Rule] = upd.map(Rule.upd)
  def toR: P[Rule] = ((string("to")~sps) *> varNames).map(ns=>
    ns.map(n=>Rule.upd(n,Term.unitT)).foldRight(Rule.empty)(_ & _))


  /// Automata Declaration ///
  def autDecl: P[AutDecl] =
    ((string("aut")~sps) *>
        varName ~ args.surroundedBy(sps) ~ parameters ~
        (((sps ~ char('{')) *> autBody.surroundedBy(sps)) <* char('}')))
      .map(p => AutDecl(p._1._1._1,p._1._1._2,p._1._2,p._2._2,p._2._1))

  def args: P0[List[String]] =
    (char('<') *> varName.surroundedBy(sps).repSep0(comma) <* char('>')).?
      .map(_.getOrElse(Nil))

  def parameters: P0[List[String]] =
    (char('(') *> varName.surroundedBy(sps).repSep0(comma) <* char(')')).?
      .map(_.getOrElse(Nil))

  private type AO = (Automaton,List[String])
  def autBody: P0[AO] =
    def mergeAuts(a1:AO, a2:AO) =
      (a1._1 & a2._1, a1._2 ::: a2._2)
    autDeclField.repSep0(sps).collect( list =>
      list.foldRight[AO]((Automaton.empty,Nil))(mergeAuts))

  def autDeclField: P[AO] =
    returnA orElse initA orElse invA orElse rulesA orElse clockA
  def returnA: P[AO] =
    (string("return")~sps~varName.repSep(comma.surroundedBy(sps))~scomma)
      .map(x => (Automaton.empty , x._1._2.toList))
  def initA: P[AO] =
    (string("init")~sps *> assgn.repSep(comma.surroundedBy(sps)) <* sps~scomma)
      .map(x => (Automaton.init(x.toList.toSet.map(Assignment.apply)),Nil))
  def invA: P[AO] = // TODO: need special terms with "at"
    (string("inv")~sps *> term.repSep(comma.surroundedBy(sps)) <* sps~scomma)
      .map(x => (Automaton.inv(x.toList.toSet),Nil))
  def rulesA: P[AO] =
    (string("rules")~sps *> rule.backtrack.repSep(sps))
      .map(x => (Automaton.rules(x.toList.toSet),Nil))
  def clockA: P[AO] =
    (string("clock")~sps~varName.repSep(comma.surroundedBy(sps))~scomma)
      .map(x => (Automaton.clocks(x._1._2.toList.toSet) , Nil))



  //// Links ////
  def link: P[LinkDecl] =
    linkArr.backtrack orElse linkAlone
  def linkArr: P[LinkDecl] =
    (inputCall ~ string("-->").surroundedBy(sps) ~ varName.repSep(comma.surroundedBy(sps)))
      .map(x => LinkDecl(x._1._1,x._2.toList))
  def linkAlone: P[LinkDecl] =
    inputCall.map(LinkDecl(_,Nil))

  def argTerms: P0[List[Term]] =
    (char('[') *> term.surroundedBy(sps).repSep0(comma) <* char(']')).?
      .map(_.getOrElse(Nil))

  def portCall: P[PortCall] =
    varName.map(PortCall.apply)

  def inputCall: P[InputCall] = P.recursive[InputCall]( recCall =>
    def inputCalls: P[(List[Term],List[InputCall])] =
      ((char('(')~sps) *> recCall.repSep0(comma.surroundedBy(sps)) <* (sps~char(')')))
        .map(Nil -> _)
    def connCall: P[ConnCall] =
      (varName ~ argTerms.surroundedBy(sps) ~ inputCalls)
        .map(x => ConnCall(x._1._1,x._1._2,x._2._2))
//    def connCall: P[ConnCall] =
//      (varName ~ (argsAndCalls orElse inputCalls)) .map(x => ConnCall(x._1,x._2._1,x._2._2))
//    def argsAndCalls: P[(List[Term],List[InputCall])] =
//      (((char('[')~sps) *> term.repSep(comma.surroundedBy(sps)) <* (sps~char(']'))) ~ inputCalls)
//        .map(x => (x._1.toList:::x._2._1 , x._2._2))

    connCall.backtrack orElse portCall
  )


