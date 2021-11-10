package dsl.revised.syntax

import cats.parse.{LocationMap, Parser => P, Parser0 => P0}
import cats.parse.Numbers._
import cats.syntax.all._
import P._
import cats.data.NonEmptyList
import cats.parse.Rfc5234.sp
import dsl.revised.Prelude
import dsl.revised.core.Network.{Constructor, Link}
import dsl.revised.core.Rule.Assignment
import dsl.revised.core.{Automaton, Rule, Term}
import dsl.revised.core.Term.{Fun, IntVal, Var}
import dsl.revised.syntax.Program
import Program.{Module,Decl,InputCall}
import Program.Decl._
import Program.InputCall._
import dsl.revised.typing.Type
import dsl.revised.typing.Type.{BaseType, VarType}

object Parser :


  def parseProgram(str:String):Program =
    program.parseAll(str) match {
      case Left(e) => //e.toString
        dsl.revised.Error.parsing(prettyError(str,e))
      case Right(p) => p
    }

  def prettyError(str:String,err:Error): String =
    val loc = LocationMap(str)
    val pos = loc.toLineCol(err.failedAtOffset) match
      case Some((x,y)) =>
        s"""at ($x,$y):
           |"${loc.getLine(x).getOrElse("-")}"
           |${"-".repeat(y+1)+"^\n"}""".stripMargin
      case _ => ""
    s"${pos}expected: ${err.expected.toList.mkString(", ")}\noffsets: ${
      err.failedAtOffset};${err.offsets.toList.mkString(",")}"


  val whitespace: P[Unit] = P.charIn(" \t\r\n").void
  val comment: P[Unit] = string("//") *> P.charWhere(_!='\n').rep0.void
  val sps: P0[Unit] = (whitespace orElse comment).rep0.void
  private[this] val comma = char(',')
  private[this] val scomma = char(';')
  private[this] val commasp = char(',').surroundedBy(sps)

  def alphaDigit: P[Char] =
    P.charIn('A' to 'Z') | P.charIn('a' to 'z') | P.charIn('0' to '9') | P.charIn('_')
  def varName: P[String] =
    (charIn('a' to 'z') ~ alphaDigit.rep0).string
  def constrName: P[String] =
    (charIn('A' to 'Z') ~ alphaDigit.rep0).string
  def symbols: P[String] = // symbols starting with "--" are meant for syntactic sugar of arrows, and ignored as sybmols of terms
    P.not(string("--")).with1 *>
    (oneOf("+-><!%/*=|&".toList.map(char)).rep).string


  //// Module body ///
  def program: P0[Program] =
    declarations.map(x => Program(dsl.revised.syntax.Program.Module(List("reo"),x),Map()))
//  def declarations: P0[List[Decl]] =
//    (data orElse autDecl orElse link).repSep0(sps).surroundedBy(sps)
  def declarations: P0[List[Decl]] =
      declaration.repSep0(sps).surroundedBy(sps).map(x => x.flatMap(y=>y._1))

  // Declaration has to be recursive, to be used within a network declaration
  private type DeclO = (List[Decl],List[String])
  def declaration: P[DeclO] = P.recursive(declRec => {

    def returnNet: P[DeclO] =
      (string("return")~sps~varName.repSep(comma.surroundedBy(sps))~scomma)
        .map(x => (Nil , x._1._2.toList))
    def declOrReturn: P[DeclO] =
      returnNet | (dataDecl | autDecl | netwDecl | constDecl | link).map(d=>(List(d),Nil))

    def declarations: P0[DeclO] =
      declRec.repSep0(sps).surroundedBy(sps)
        .map(lst => {
          val (x, y) = lst.unzip
          (x.flatten, y.flatten)
        })
    def netwDecl: P[NetDecl] =
      ((string("def")~sps) *>
        varName ~ args.surroundedBy(sps) ~ parameters ~
          (((sps ~ char('{')) *> declarations.surroundedBy(sps)) <* char('}')))
        .map(p => NetDecl(p._1._1._1,p._1._1._2,p._1._2,p._2._2,p._2._1))

    declOrReturn
  })

  /// Terms ///
  def term: P[Term] = P.recursive[Term] { newTerm =>
    def funArgs: P[List[Term]] =
      (char('(') *> newTerm.surroundedBy(sps).repSep0(comma) <* char(')'))
    def constrArgs: P0[List[Term]] = // optional arguments in constructors
      funArgs.?.map(_.getOrElse(Nil))
    def constrTerm: P[Term] =
      ((constrName <* sps) ~ constrArgs).map(Fun.apply)
    def funTerm: P[Term] =
      ((varName <* sps) ~ funArgs).map(Fun.apply)
    def infixTerm: P[Term] = // TODO: missing precedence of symbols - later?
      (simpleTerm ~ symbols.surroundedBy(sps) ~ newTerm).map(x=>Fun(x._1._2,List(x._1._1,x._2)))
    def simpleTerm: P[Term] =
      unitTerm | paren | intTerm | constrTerm | funTerm.backtrack | varTerm
    def unitTerm: P[Term] =
      string("()").map(_=>Fun("()",Nil))
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
    val rl = (labels.? ~ sps ~ guards).with1 ~ arr ~ ruleActs ~ char(';')
    rl.map(x => x._1._1._1._1._1.getOrElse(Rule.empty) & x._1._1._1._2 --> x._1._2)

  def varNames: P[Set[String]] =
    ((char('(') *> varName.surroundedBy(sps).repSep(comma)) <* char(')'))
      .map(_.toList.toSet)

  def labels: P0[Rule] = (char('[') *> P.charWhere(_!=']').rep0 <* char(']'))
    .map(x => Rule.lbls(x.mkString.split(";").toSet.map(_.trim)))

  def guards: P0[Rule] = (guard <* sps).repSep0(char(',')~sps)
    .collect{ case x => x.foldRight(Rule.empty)(_ & _) }
  def guard: P[Rule] = get | ask | und | term.map((t:Term)=>Rule.pred(t))
  def get: P[Rule] = ((string("from")~sps) *> varNames).map(Rule.get)
  def ask: P[Rule] = ((string("at")~sps) *> varNames).map(Rule.ask)
  def und: P[Rule] = ((string("notAt")~sps) *> varNames).map(Rule.und)

  def assgn:   P[(String,Term)] = (varName <* (string(":=")).surroundedBy(sps)) ~ term
  def assgnEq: P[(String,Term)] = (varName <* (string("=")).surroundedBy(sps)) ~ term
//  def assgnEq: P[(String,Term)] = (varName <* (char('\'')~sps~string(":=")).surroundedBy(sps)) ~ term
  def atInit:    P[(String,Term)] = (string("at")~char('(').surroundedBy(sps)~varName~sps~char(')'))
    .map(x => (x._1._1._2, Term.unitT))
  def assgnInit:P[(String,Term)] = atInit.backtrack | assgn

  def ruleActs: P0[Rule] = (ruleAct <* sps).repSep0(char(',')~sps)
    .collect{case x => x.foldRight(Rule.empty)(_ & _)}
  def ruleAct: P[Rule] = toR.backtrack orElse assgnR.backtrack orElse updR
  def assgnR: P[Rule] = assgnEq.map(Rule.eqs)
  def updR: P[Rule] = assgn.map(Rule.upd)
  def toR: P[Rule] = ((string("to")~sps) *> varNames).map(ns=>
    ns.map(n=>Rule.upd(n,Term.unitT)).foldRight(Rule.empty)(_ & _))


  /// Automata Declaration ///
  def autDecl: P[AutDecl] =
    ((string("aut")~sps) *>
        varName ~ args.surroundedBy(sps) ~ parameters ~
        (((sps ~ char('{')) *> autBody.surroundedBy(sps)) <* char('}')))
      .map(p => AutDecl(p._1._1._1,p._1._1._2,p._1._2,p._2._2,p._2._1))

  def args: P0[List[String]] =
    (char('[') *> varName.surroundedBy(sps).repSep0(comma) <* char(']')).?
      .map(_.getOrElse(Nil))

  def parameters: P0[List[String]] =
    (char('(') *> varName.surroundedBy(sps).repSep0(comma) <* char(')')).?
      .map(_.getOrElse(Nil))

  private type AO = (Automaton,List[String])
  def autBody: P0[AO] =
    def mergeAuts(a1:AO, a2:AO) =
      (a1._1 & a2._1, a1._2 ::: a2._2)
    autDeclField.repSep0(sps).collect{ case list =>
      list.foldRight[AO]((Automaton.empty,Nil))(mergeAuts)}

  def autDeclField: P[AO] =
    returnA orElse initA orElse invA orElse rulesA orElse clockA
  def returnA: P[AO] =
    (string("return")~sps~varName.repSep(comma.surroundedBy(sps))~scomma)
      .map(x => (Automaton.empty , x._1._2.toList))
  def initA: P[AO] = // TODO: need special terms with "at" and "notAt"
    (string("init")~sps *> assgnInit.repSep(comma.surroundedBy(sps)) <* sps~scomma)
      .map(x => (Automaton.init(x.toList.toSet.map(Assignment.apply)),Nil))
  def invA: P[AO] = // TODO: need special terms with "at" and "notAt"
    (string("inv")~sps *> term.repSep(comma.surroundedBy(sps)) <* sps~scomma)
      .map(x => (Automaton.inv(x.toList.toSet),Nil))
  def rulesA: P[AO] =
    (string("rules")~sps *> rule.backtrack.repSep(sps))
      .map(x => (Automaton.rules(x.toList.toSet),Nil))
  def clockA: P[AO] =
    (string("clock")~sps~varName.repSep(comma.surroundedBy(sps))~scomma)
      .map(x => (Automaton.clocks(x._1._2.toList.toSet) , Nil))

  //// Constant ///
  def constDecl: P[ConstDecl] =
    (string("const")~varName.surroundedBy(sps)~
      char('=')~term.surroundedBy(sps)~scomma)
      .map(x => ConstDecl(x._1._1._1._2, x._1._2))

  //// Data ///
  def dataDecl: P[DataDecl] =
    (string("data")~constrName.surroundedBy(sps)~
      parameters~char('=').surroundedBy(sps)~
      dataConstr.repSep(char('|').surroundedBy(sps))
      <* (sps~scomma))
      .map(x => DataDecl(x._1._1._1._2, x._1._1._2, x._2.toList))

  def dataConstr: P[Constructor] =
    (constrName~(sps*>typeParams.?))
      .map(x => Constructor(x._1,x._2.getOrElse(Nil)))

  def typeParams: P[List[Type]] = P.recursive(recTypeParams => {
    def typeDecl: P[Type] =
      varName.map(VarType(_)) orElse
      (constrName~sps~recTypeParams.?)
        .map(x => BaseType(x._1._1,x._2.map(_.toList).getOrElse(Nil)))

    (char('(') *> typeDecl.surroundedBy(sps).repSep(comma) <* char(')'))
      .map((x:NonEmptyList[Type]) => x.toList)
  })

  //// Links ////
  def link: P[LinkDecl] =
    (linkArr.backtrack orElse linkAlone) <* (sps~scomma)
  def linkArr: P[LinkDecl] =
    (inputCall ~ arrowCall.surroundedBy(sps) ~ varName.repSep(comma.surroundedBy(sps)))
      .map(x => x._1._2(x._1._1,x._2.toList))
  private type I2Link = (InputCall,List[String])=>LinkDecl
  def arrowCall: P[I2Link] =
    // note: order is important (first one to match wins)
    string("-->").map[I2Link](_ => (i,o)=>LinkDecl(i,o)) |
    mkLink("fifo",string("--[]-->").map(_=>Nil)).backtrack |
    mkLink("fifofull",string("--[")*>term.surroundedBy(sps).map(List(_))<*string("]-->")).backtrack |
    mkLink("var",string("--{}-->").map(_=>Nil)).backtrack |
    mkLink("varfull",string("--{")*>term.surroundedBy(sps).map(List(_))<*string("}-->")).backtrack |
    mkLink("xor",string("--X-->").map(_=>Nil)).backtrack |
    string("---").map[I2Link](_ => ((i,o) =>
        LinkDecl(ConnCall("drain",Nil,i::o.map(PortCall.apply)),Nil)))
  def mkLink(name:String,pattern:P[List[Term]]): P[I2Link] =
    pattern.map(terms => ((in,outs) => LinkDecl(ConnCall(name,terms,List(in)),outs)))


  def linkAlone: P[LinkDecl] =
    inputCall.map(LinkDecl(_,Nil))

  def argTerms: P0[List[Term]] =
    (char('[') *> term.surroundedBy(sps).repSep0(comma) <* char(']')).?
      .map(_.getOrElse(Nil))

  def portCall: P[PortCall] =
    varName.map(PortCall.apply)

  def inputCall: P[InputCall] = P.recursive[InputCall]( recCall => {
    def inputCalls: P[(List[Term], List[InputCall])] =
      ((char('(') ~ sps) *> recCall.repSep0(comma.surroundedBy(sps)) <* (sps ~ char(')')))
        .map(Nil -> _)

    def connCall: P[ConnCall] =
      (varName ~ argTerms.surroundedBy(sps) ~ inputCalls)
        .map(x => ConnCall(x._1._1, x._1._2, x._2._2))

    connCall.backtrack orElse portCall
  })



  object Examples:
    val ex1 =
      """
        |data CtrCom = Start | Left | Right | Stop | Reset;
        |data Heartbeat = Tick;
        |data Error = BadDec | BadMon; // ...
        |
        |
        |// receives a stream of CtrCom
        |// inferred types: forall a. actions:a?, outA:a!, outT:Heartbeat!
        |aut dash(actions) {
        |  clock t;
        |  init ticks:=0, working:=();
        |
        |  inv at(working) -> (t<=16); // NOT SUPPORTED in uppaal: (t<=16) || notAt(working);
        |
        |  rules
        |  // Either send a tick or an action based on MaxTicks(42) and period TDashMin-TDashMax (15-16)
        |  [sendAct] at(working), t>=15, from(actions), ticks==42 -->
        |              outA=actions, ticks:=0, t:=0;
        |  [sendTick] at(working), t>=15, ticks!=42 --> outT=Tick, ticks:=ticks+1, t:=0;
        |  [NoMoreActs] from(working), t==16, ticks==42, notAt(actions) --> ;
        |
        |  return outA, outT;
        |}
        |
        |// Decode and sync with MinSyn-MaxSyn period (22-23), check with decoding time of MinDec-MaxDec (12-13) and error after MaxTries (03)
        |aut decoder(inAct, beat, otherDec) {
        |  clock dec, syn, tk;
        |
        |  init  at(waiting); // waiting := ();
        |
        |  inv at(decode) -> (dec<=13),
        |      syn<=23,
        |      tk<=16;
        |
        |  rules
        |  // getting actions and sending them after MinDec time
        |  from(waiting,inAct) --> a:=inAct, to(decode);
        |  from(decode,a), dec>=22 --> outAct=a, to(waiting), dec:=0;
        |
        |  // checking if the last received message is the same as `other` every MinSyn
        |  syn>=22, from(other), other!=a, tries!=03
        |    --> syn:=0, tk:=0, outWarn=BadDec, tries:=tries+1;
        |  syn>=22, from(other), other!=a, tries==03
        |    --> syn:=0, tk:=0, outErr=BadDec;
        |  syn>=22, from(other), other==a
        |    --> syn:=0, tk:=0, tries:=0;
        |
        |  // checking and forwarding heartbeats
        |  tk>=15, from(beat) --> outBeat=beat, tk:=0;
        |
        |  return outAct, outBeat, outWarn, outErr; // types can be inferred
        |}
        |
        |//stream myActions<Start,Left,Right,...>
        |//stream timeActs<   100,  10, 25  ,...>
        |
        |aut actions {
        |  init m1:=Start, m2:=Left; //, ...
        |  rules [Act1] from(m1) --> x=m1;
        |        [Act2] from(m2), notAt(m1) --> x=m2;
        |  return x;
        |}
        |
        |aut monitor(in,otherMon) { rules from(in) --> out=in; return out, a, b;} // todo
        |aut controller(act,err,rep) { return stat,comm;} // todo
        |
//        | decoder(toDec1,hb,lastAct) --> act1,hb1,warn1,err1;
//        | actions() --> x;
//        | dash(x) --> action,hb;
        |// Dashboard produces actions
        |dash(actions()) --> action,hb;
        |action --> toDec1;
        |action --> toDec2;
        |
        |// decoders produce actions, heartbeats, warnings, and errors
        |decoder(toDec1,hb,lastActD2) --> actD1,hb1,warnD1,errToC1;
        |decoder(toDec2,hb,lastActD1) --> actD2,hb2,warnD2,errToC2;
        |actD1 --{Stop}--> lastActD1;
        |actD2 --{Stop}--> lastActD2;
        |
        |// controllers produce stateUpd and motorCommands
        |controller(actD1,errToC1,lastRepByRd1) --> statC1, commC1;
        |controller(actD2,errToC2,lastRepByRd2) --> statC2, commC2;
        |
        |// monitors forward actions and send warnings and errors
        |monitor(statC1,lastStatM2) --> statM1,warnM1,errToC1;
        |monitor(statC2,lastStatM1) --> statM2,warnM2,errToC2;
        |statM1 --{}--> lastStatM1;
        |statM2 --{}--> lastStatM2;
        |""".stripMargin


