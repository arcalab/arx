package dsl.analysis.syntax

import dsl.analysis.syntax.SymType._
import dsl.analysis.syntax.ast._


import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

/**
  * Created by guillecledou on 2019-07-16
  */

object NewParser extends RegexParsers with preo.lang.Parser {
  private var adts:List[TypeDeclaration] = List()
  private var constr:List[String] = List()
  private var sym: SymbolsTable = new SymbolsTable

  def parseProgram(code:String):ParseResult[Program] = {
    adts = List()
    constr = List()
//    conns = List()
    sym = new SymbolsTable
    parseAll(program,code)
  }

  override def skipWhitespace = true
  override val whiteSpace: Regex = "( |\t|\r|\f|\n|//.*)+".r

  val capitalId:Parser[String] = """[A-Z][a-zA-Z0-9_]*""".r
  val absTypeId:Parser[String] = """[a-z][a-zA-Z0-9_]*""".r //possible can just be represented as a regular typename
  val id:Parser[String] = """[a-zA-Z][a-zA-Z0-9_]*""".r

  override val keywords: Set[String] = Set("def","match","build","True","False","Left","Right","U")

  /* Program */

  def program:Parser[Program] =
    rep(dt) ~
//    rep(funDef) ~
    rep(streamExpr) ^^ { case tds~ses => Program(tds, ses) }

  /* Type declaration */

  // data type declaration
  def dt:Parser[TypeDeclaration] =
    "data" ~ capitalId ~ opt(tparams) ~ "=" ~ constructors ^^
      { case _~n~tps~_~vs => sym=sym.add(n,TYPE);val td = TypeDeclaration(ConTypeName(n,tps.getOrElse(List())),vs) ; adts::=td ; td}
  // data type variants
  def constructors: Parser[List[Constructor]] =
    constructor ~ opt("|" ~> constructors) ^^ {case v~vs => v::vs.getOrElse(List())}
  // constructor definition
  def constructor:Parser[Constructor] =
    capitalId ~ opt("(" ~> tnames <~ ")")^^ {case n~tps => sym=sym.add(n,CONST); Constructor(n,tps.getOrElse(List()))}
  // a non empty sequence of type names
  def tnames:Parser[List[TypeName]] =
    tname ~ rep("," ~> tname) ^^ {case n~ns => n::ns}
  // a type name
  def tname:Parser[TypeName] =
    capitalId ~ opt(tparams)  ^^ {case n~opt =>  ConTypeName(n,opt.getOrElse(List()))} |
      absTypeId ^^ {case n => AbsTypeName(n)}
  // type parameters
  def tparams:Parser[List[TypeName]] =
    "<" ~> tnames <~ ">"


  /* Connector definitions */

//  def connDef:Parser[ConnDef] =
//    "def" ~ id ~ "="~"{" ~ preo ~ "}" ^^ {
//      case _~id~_~_~conn~_ => sym=sym.add(id,CONNNAME); conns::=ConnDef(id,conn); ConnDef(id,conn)
//    }

  /* Function definitions */
  def funDef: Parser[FunDefinition] =
    "def" ~> restFunDef


  def restFunDef:Parser[FunDefinition] = {
    sym = sym.addLevel()
    id ~ opt(tparams) ~ opt("<" ~> dataFormalParams <~ ">") ~ funParamsAndBody ^^ {
      case f ~ tps ~ dps ~ r => r match {
        case Right(sf) => {
          sym = sym.rmLevel()
          sym = sym.add(f, FUN);
          FunSFDef(f, tps.getOrElse(List()), dps.getOrElse(List()), sf)
        }
        case Left((ps, se)) => {
          sym = sym.rmLevel()
          sym = sym.add(f, FUN);
          FunSEDef(f, tps.getOrElse(List()), dps.getOrElse(List()), ps, se)
        }
      }
    }
  }

  def funParamsAndBody:Parser[Either[(List[Variable],StreamExpr),StreamFun]] =
    "(" ~ opt(funFormalParams) ~ ")" ~ "=" ~"{"~ streamExpr ~ "}" ^^ { case _~ps~_~_~_~e~_ => Left((ps.getOrElse(List()),e))} |
    "=" ~ "{" ~ streamFun ~ "}" ^^ {case _~_~sf~_ => Right(sf)}

//  def funParams:Parser[Option[List[Variable]]] =

  // comma separated list of identifiers
  def funFormalParams: Parser[List[Variable]] =
    id ~ rep(","~> id) ^^ {
      case id~ids => (id::ids).map(i => {sym=sym.add(i,INPUT); Variable(i)})}

  def dataFormalParams: Parser[List[Variable]] =
    id ~ rep(","~> id) ^^ {
      case id~ids => (id::ids).map(i => {sym=sym.add(i,DATA); Variable(i)})}

  /* Assignments */

  def assignment:Parser[StreamExpr] =
    id ~ rep("," ~> id) ~ ":=" ~ streamExpr ~ "." ^^ {
      case i~Nil~_~e~_ => sym=sym.add(i,VAR); Assig(List(Variable(i)),e)
      case i~ids~_~e~_ =>
        (i::ids).foreach(i => sym=sym.add(i,VAR))
        Assig((i::ids).map(Variable),e)
    }

  /* Stream Expressions */

  def streamExpr:Parser[StreamExpr] =
    simpleStreamExpr ~ opt(streamExpr) ^^ {
      case e1 ~ Some(e2) => ParExpr(e1, e2)
      case e1 ~ None => e1
    }

  def simpleStreamExpr:Parser[StreamExpr] =
    funDef |
    id ~ groundParams ^^ {
      case n~ps if sym.contains(n) && sym(n).get == CONST => ConstExpr(n,ps)
      case n~ps  => FunExpr(FunName(n),ps)
    } |
    streamFun ~ groundParams ^^ {case sf~ps => FunExpr(sf,ps)} |
    assignment |
    id ^^ (n => sym(n) match {
      case Some(CONST) => ConstExpr(n)
      case _ => sym =sym.add(n,VAR) ;Variable(n)
    })

  /* Stream Functions */

  /**
    * Parses a stream function: sequence of functions, functions in parallel,
    * or a simple stream function [[simpleStreamFun]]
    * @return a stream function
    */
  def streamFun:Parser[StreamFun] =
    simpleStreamFun ~ ";" ~ streamFun ^^ {case sf1~_~sf2 => SeqFun(sf1,sf2)} |
    simpleStreamFun ~ streamFun ^^ {case sf1~sf2 => ParFun(sf1,sf2)}|
    simpleStreamFun

  /**
    * Parses a simple stream function: match, build or a fun name
    * todo: check that the fun name is not register as a variable or constructor
    * @return a stream function
    */
  def simpleStreamFun:Parser[StreamFun] =
    "build" ~ opt("<" ~> tname <~ ">") ^^ {case _~ps => Build(ps)} |
    "match" ~ opt("<" ~> tname <~ ">") ^^ {case _~ps => Match(ps)} |
    id ^^ (n => FunName(n))

  /* Ground terms */

  /**
    * Parses a ground term: either a constructor or a variable
    * todo: could check if the constructor exists, otherwise error
    * @return a ground term
    */
  def groundTerm:Parser[GroundTerm] =
    capitalId ~ groundParams  ^^ { case n~ps => ConstExpr(n,ps)} |
    id ^^ { i => sym=sym.add(i,VAR); Variable(i)}

  /**
    * Parses parameters which can be ground terms only
    * @return list of ground terms
    */
  def groundParams:Parser[List[GroundTerm]] =
    "(" ~ groundTerm ~ rep("," ~> groundTerm) ~ ")" ^^ {case _~g~gs~_ => g::gs}
}
