package dsl.analysis.syntax

import dsl.analysis.semantics._
import dsl.analysis.syntax.Program.Block
//import dsl.analysis.syntax.SymbolType._
import dsl.analysis.types.{Pull, Push}
import dsl.backend.{Import, Prelude}

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.{Position, Positional}

/**
  * Created by guillecledou on 2019-05-31
  */

object Parser extends RegexParsers {

  //private var sym: SymbolsTable = new SymbolsTable
  //todo: fix error with fun def not creating new level in symbols table
  def parseProgram(code:String) = {
    //sym = new SymbolsTable
    parseAll(program,code)
  }

  def parseFunction(code:String) = {
    //sym = new SymbolsTable
    parseAll(funDef,code)
  }

  override def skipWhitespace = true
  override val whiteSpace: Regex = "( |\t|\r|\f|\n|//.*)+".r

  val upId:Parser[String] = """[A-Z][a-zA-Z0-9_]*""".r
  val lowId:Parser[String] = """[a-z][a-zA-Z0-9_]*""".r //possible can just be represented as a regular typename
  val id:Parser[String] = """[a-zA-Z][a-zA-Z0-9_]*""".r
  val num:Parser[Int] = """[0-9][0-9]*""".r ^^ (n => n.toInt)

  // todo: use it
  val keywords: Set[String] =
    Set("data","def","match","build") ++ Prelude.primitiveFunctionNames()

  /* Program */

  def program:Parser[Program] =
    rep(positioned(imports)) ~
    rep(positioned(dt)) ~ block ^^ {
      case imp~typs~bl => Program(imp,typs,bl)}

  def imports:Parser[Import] =
    "import"~upId~rep("."~>upId)~opt(members) ^^ {
      case _~m~ms~mem => Import((m::ms).mkString("."),mem.getOrElse(List()))
    }

  def members:Parser[List[String]] =
    ".{"~>repsep(id,",")<~"}" |
    "."~>id ^^ {List(_)}

  def block: Parser[Block] =
    rep(positioned(statement))

  def statement: Parser[Statement] = {
    funDef | sbDef | assignment | strExpr
  }

  def strExpr: Parser[StreamExpr] =
    positioned(strFun)~args ^^ {
      case f~as => FunctionApp(f,as)
    } |
    ground

  def args: Parser[List[GroundTerm]] =
    "("~>repsep(ground,",")<~")"

  def ground: Parser[GroundTerm] =
    lowId ^^ Port.apply |
    groundQs


  def groundQs:Parser[Const] =
    upId ~ opt(args) ^^ {
      case q ~ as => Const(q, as.getOrElse(Nil))
    }

  def strFun: Parser[StreamFun] =
    "build" ^^^ Build |
    "match" ^^^ Match |
    groupStrFun
//    lowerCaseId ^^ FunName |
    // TODO: sequential and parallel composition

  //      rep(connDef) ~
//      rep(funDef) ~
//      rep(assignment) ^^ {
//        case tds~conn~fun~asg => Statements(tds ++ conn ++ fun ++ asg)
//      }

  def groupStrFun:Parser[StreamFun] =
    lowId ~ opt( "<" ~> rep1sep(groundQs,",") <~ ">")^^ {
      case f~qs => FunName(f,qs.getOrElse(List()))} |
    "{" ~> parOrSeq <~ "}"


  def parOrSeq:Parser[StreamFun] =
    groupStrFun ~ ";" ~ parOrSeq ^^ {case f1 ~_~f2 => SeqFun(f1,f2)} |
    groupStrFun ~ opt(parOrSeq) ^^ {case f1~f2 => if (f2.isDefined) ParFun(f1,f2.get) else f1}

  /* Type declaration and type parameters */

  def dt:Parser[TypeDecl] =
    "data" ~ upId ~ opt(tparams) ~ "=" ~ constructors ^^ {
      case _~n~tps~_~vs => /*sym=sym.add(n,TYPE);*/TypeDecl(ConTypeName(n,tps.getOrElse(List())),vs)}

  // constructors
  def constructors: Parser[List[Constructor]] =
    constructor ~ opt("|" ~> constructors) ^^ {case v~vs => v::vs.getOrElse(List())}

  // constructor
  def constructor:Parser[Constructor] =
    upId ~ opt("(" ~> tnames <~ ")")^^ {
      case n~tps => /*sym=sym.add(n,CONST);*/ Constructor(n,tps.getOrElse(List()))}

  // a non empty sequence of type names
  def tnames:Parser[List[TypeName]] =
    tname ~ rep("," ~> tname) ^^ {case n~ns => n::ns}

  // a type name
  def tname:Parser[TypeName] =
    upId ~ opt(tparams)  ^^ {case n~opt =>  ConTypeName(n,opt.getOrElse(List()))} |
    lowId ^^ (n => AbsTypeName(n))

  // type parameters
  def tparams:Parser[List[TypeName]] =
    "<" ~> tnames <~ ">"

  /* Function definition */
  def funDef: Parser[Statement] =
    "def" ~> restFunDef //log(restFunDef)("restFunDef")


  def restFunDef:Parser[Statement] = {
//    sym = sym.addLevel()
    //println("entering fun def")
    lowId /* ~ opt("<" ~> dataFormalParams <~ ">")*/ ~ funParamsAndBody ^? ({
      case f ~ rest  if !keywords.contains(f) =>
//        sym = sym.add(f, FUN) // quick hack to see if function name is not used/delared inside the function todo: fix it
//        sym = sym.rmLevel()
//        sym = sym.add(f, FUN) // actually added it to the current scope
        rest match {
          case Right((typ,sf)) => SFunDef(f, typ, sf)
          case Left((ps,typ, bl)) => FunDef(f,ps, typ, bl)}},
      {case f~_ => /*sym=sym.rmLevel();*/ s"Cannot use the reserve word $f as a function name"})
  }

  def funParamsAndBody:Parser[Either[(List[TypedVar],Option[TypeName],Block),(Option[TypeName],StreamFun)]] =
    "(" ~ opt(funFormalParams) ~ ")" ~ opt(":"~>tname) ~ "=" ~ "{" ~ block ~ "}" ^^
      {case _~ps~_~typ~_~_~bl~_ => Left((ps.getOrElse(List()),typ,bl))} |
     opt(":"~>tname) ~ "=" ~ "{" ~ strFun ~ "}" ^^
      {case typ~_~_~sf~_ => Right((typ,sf))}

  // todo: dropped for now until we solve it in the paper
  def dataFormalParams: Parser[List[Port]] =
    id ~ rep(","~> id) ^^ { case id~ids => (id::ids).map(i => {/*sym=sym.add(i,DATA); */Port(i)})}

  // comma separated list of identifiers
  def funFormalParams: Parser[List[TypedVar]] =
    repsep(typedVar,",") ^^ { tvs =>
      /*tvs.foreach(tv => sym = sym.add(tv.name,INPUT))*/ //todo: Possibly no need of using INPUT, just use VAR
      tvs
    }

  def typedVar: Parser[TypedVar] =
    lowId~opt(":"~>tname)~opt("[!|?]".r) ^^ {
      case v~t~None => TypedVar(v,t,None)
      case v~t~Some(r) => TypedVar(v,t,if (r.matches("!")) Some(Push) else Some(Pull))
    }

  /* Assignments */

  def assignment:Parser[Statement] =
    rep1sep(lowId,",")~"<-|<~".r~strExpr ^^ {
      case ids~typ~expr =>
//        ids.foreach(id => sym = sym.add(id,VAR))
        if (typ.matches("<-")) Assignment(ids.map(Port(_)), expr)
        else RAssignment(ids.map(Port(_)),expr)
    }

  /* Stream builder definition */

  /**
    * Stream builder definition parser
    * @return a stream builder definition statement
    */
  def sbDef:Parser[Statement] = {
//    sym = sym.addLevel()
    var name = ""
    val res = "sb"~lowId~opt(memories)~opt(formalParams)~"="~"{"~opt(initSB)~rep1(gc)~opt(rep(lowId))~"}" ^^ {
      case _~id~mems~fp~_~_~init~gcs~outs~_ =>
//        sym = sym.add(id, FUN)
        name = id
        SBDef(id,mems.getOrElse(List()),fp.getOrElse(List()),init.getOrElse(List()),gcs.toSet,outs.getOrElse(List()))
    }
//    sym = sym.rmLevel()
//    if (name.nonEmpty) sym = sym.add(name, FUN)
    res
  }

  /**
    * Stream builder memory declaration parser
    * @return a list of [typed] memory variables
    */
  def memories:Parser[List[TypedVar]] =
    "<"~>typedParams<~">"

  /**
    * Stream builder formal parameter parser
    * @return a list of [typed] input variables
    */
  def formalParams:Parser[List[TypedVar]] =
    "("~>typedParams<~")"

  /**
    * List of [typed] variables parser
    * @return a list of [typed] variables
    */
  def typedParams:Parser[List[TypedVar]] =
    repsep(typedVar,",") ^^ { tvs =>
//      tvs.foreach(tv => sym = sym.add(tv.name,INPUT))
      tvs
    }

  /**
    * Stream builder initial state parser
    * @return a set of commands initializing variables
    */
  def initSB:Parser[List[Command]] =
    "init" ~> rep1sep(cmd, ",")

  /**
    * Stream builder command parser
    * @return a command
    */
  def cmd:Parser[Command] =
    lowId~":="~sbterm ^^ { case v~_~t => Command(v,t)}

  /**
    * Stream builder term parser
    * @return a term, either a variable, destructor, or a constructor
    */
  def sbterm: Parser[Term] =
    variable | sbdestructor | sbconstructor

  /**
    * Stream Builder variable parser
    * @return a variable
    */
  def variable:Parser[Term] =
    lowId ^^ { v => /*sym = sym.add(v, INPUT); */Var(v) }

  /**
    * Stream builder destructor parser
    * @return a destructor of a constructor, based on an index and a term
    */
  def sbdestructor:Parser[Term] =
    "Get"~"<"~upId~","~num~">"~"("~sbterm~")" ^^ {
      case _~_~tp~_~i~_~_~t~_ => GetQ(tp,i,t)
    }

  /**
    * Stream builder constructor parser
    * @return a constructor
    */
  def sbconstructor:Parser[Term] =
    upId~opt("("~>rep1(sbterm)<~")") ^^ {case v~ts => Q(v,ts.getOrElse(List()))} |
      "_" ^^ {case _ => Q("_",List())} // ss for () or don't care?

  /**
    * Guarded command parser
    * @return a guarded command
    */
  def gc:Parser[GuardedCommand] =
    guard~"->"~repsep(cmd,",") ^^ {case g~_~cs => (g --> cs.toSet)}

  /**
    * Stream builder guard parser
    * @return a guard
    */
  def guard:Parser[Guard] =
    repsep(guardItem,",") ^^ (gs => Guard(gs.foldRight[Set[GuardItem]](Set())(_.union(_))))

  /**
    * Guard item parser
    * @return a get,und,isQ,or ask guard
    */
  def guardItem:Parser[Set[GuardItem]] =
    gets | asks | isQs | unds

  /**
    * Get guard parser
    * @return a get guard over one or more variabless
    */
  def gets:Parser[Set[GuardItem]] =
    "get" ~> guardParam ^^ (ps => ps.map(s => Get(s)))

  /**
    * Ask guard parser
    * @return an ask guard over one or more variables
    */
  def asks:Parser[Set[GuardItem]] =
    "ask" ~> guardParam ^^ (ps => ps.map(s => Ask(s)))


  /**
    * Und guard parser
    * @return an und guard over one or more variables
    */
  def unds:Parser[Set[GuardItem]] =
    "und" ~> guardParam ^^ (ps => ps.map(s => Und(s)))

  /**
    * IsQ guard parser
    * @return an isQ guard over one or more variables
    */
  def isQs:Parser[Set[GuardItem]] =
    "is" ~ upId ~ "(" ~ sbterm ~ ")" ^^ { case _ ~ v ~ _ ~ t ~ _ => Set(IsQ(v, t)) }

  /**
    * Guard parameter parser
    * @return a non-empty set of variables
    */
  def guardParam:Parser[Set[String]] =
    "("~>rep1sep(lowId,",")<~")" ^^ { v => /*v.foreach(p => sym = sym.add(p, INPUT));*/ v.toSet }

}
