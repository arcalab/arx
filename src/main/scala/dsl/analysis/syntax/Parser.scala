package dsl.analysis.syntax

import dsl.analysis.syntax.SymType._
import dsl.analysis.syntax.ast._


import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

/**
  * Created by guillecledou on 2019-07-16
  */

object Parser extends RegexParsers {
  private var adts:List[TD] = List()
  private var constr:List[String] = List()
  private var sym: SymbolsTable = new SymbolsTable

  def parseProgram(code:String):ParseResult[Prog] = {
    sym = new SymbolsTable
    parseAll(program,code)
  }

  override def skipWhitespace = true
  override val whiteSpace: Regex = "( |\t|\r|\f|\n|//.*)+".r

  val capitalId:Parser[String] = """[A-Z][a-zA-Z0-9_]*""".r
  val lowerCaseId:Parser[String] = """[a-z][a-zA-Z0-9_]*""".r //possible can just be represented as a regular typename
  val id:Parser[String] = """[a-zA-Z][a-zA-Z0-9_]*""".r

//  val keywords: Set[String] = Set("def","match","build","True","False","Left","Right","U")

  /* Program */

  def program:Parser[Prog] =
    rep(dt) ~ se ^^ { case tds~e => Prog(tds, e) }

  /* Type declaration and type parameters */

  def dt:Parser[TD] =
    "data" ~ capitalId ~ opt(tparams) ~ "=" ~ constructors ^^ {
      case _~n~tps~_~vs => sym=sym.add(n,TYPE);TD(ConTN(n,tps.getOrElse(List())),vs)}

  // constructors
  def constructors: Parser[List[Constructor]] =
    constructor ~ opt("|" ~> constructors) ^^ {case v~vs => v::vs.getOrElse(List())}

  // constructor
  def constructor:Parser[Constructor] =
    capitalId ~ opt("(" ~> tnames <~ ")")^^ {
      case n~tps => sym=sym.add(n,CONST); Constructor(n,tps.getOrElse(List()))}

  // a non empty sequence of type names
  def tnames:Parser[List[TypeName]] =
    tname ~ rep("," ~> tname) ^^ {case n~ns => n::ns}

  // a type name
  def tname:Parser[TypeName] =
    capitalId ~ opt(tparams)  ^^ {case n~opt =>  ConTN(n,opt.getOrElse(List()))} |
      lowerCaseId ^^ {case n => AbsTN(n)}

  // type parameters
  def tparams:Parser[List[TypeName]] =
    "<" ~> tnames <~ ">"

  /* Function definitions */

  def funDef: Parser[SE] =
    "def" ~>  restFunDef

  def restFunDef:Parser[SE] = {
    sym = sym.addLevel()
    id ~ opt(tparams) ~ opt("<" ~> dataFormalParams <~ ">") ~ funParamsAndBody ^^ {
      case f ~ tps ~ dps ~ r =>
        sym = sym.add(f, FUN) // quick hack to see if function name is not used/delared inside the function todo: fix it
        sym = sym.rmLevel()
        sym = sym.add(f, FUN) // actually added it to the current scope
        r match {
        case Right(sf) => SFDef(f, tps.getOrElse(List()), dps.getOrElse(List()), sf)
        case Left((ps, se)) => SEDef(f, tps.getOrElse(List()), dps.getOrElse(List()), ps, se)}}
  }

  def funParamsAndBody:Parser[Either[(List[Id],SE),SF]] =
    "(" ~ opt(formalParams) ~ ")" ~ "=" ~"{"~ se ~ "}" ^^ {
      case _~ps~_~_~_~e~_ => println("found sed");Left((ps.getOrElse(List()),e))} |
    "=" ~ "{" ~ sf ~ "}" ^^ {
      case _~_~sf~_ => println("found sfd");Right(sf)}

  def formalParams: Parser[List[Id]] =
    id ~ rep(","~> id) ^^ { case id~ids => (id::ids).map(i => {sym=sym.add(i,INPUT); Id(i)})}

  def dataFormalParams: Parser[List[Id]] =
    id ~ rep(","~> id) ^^ { case id~ids => (id::ids).map(i => {sym=sym.add(i,DATA); Id(i)})}

  /* Assignments */

  def assignment:Parser[SE] =
    id ~ rep("," ~> id) ~ ":=" ~ se ~ "." ^^ {
      case i~Nil~_~e~_ => sym=sym.add(i,VAR); Asg(List(Id(i)),e)
      case i~ids~_~e~_ =>
        (i::ids).foreach(i => sym=sym.add(i,VAR))
        Asg((i::ids).map(Id),e)
    }

  /* Stream Expressions */

  def se:Parser[SE] =
    oneSe ~ opt(se) ^^ {
      case e1 ~ Some(e2) => ParSE(e1, e2)
      case e1 ~ None => e1
    }

  def oneSe:Parser[SE] =
    funDef |
    assignment |
    fbuild ~ "("~ groundParams ~ ")" ^^ {case b~_~ps~_ => SFExpr(b,ps)} |
    fmatch ~ "("~ groundParams ~ ")" ^^ {case b~_~ps~_ => SFExpr(b,ps)} |
    id ~ "("~ groundParams ~ ")" ^^ {case i~_~ps~_ => IdArgs(i,ps)} |
    id ^^ {i => if (!sym(i).contains(CONST)) sym=sym.add(i,VAR); Id(i)} | // if n is not a constructor, add it to the context
    sf ~ "("~ groundParams ~ ")" ^^ {case i~_~ps~_ => SFExpr(i,ps)}

  /* Stream Functions */

  /**
    * Parses a stream function: sequence of functions, functions in parallel,
    * or a simple stream function
    * @return a stream function
    */
  def sf:Parser[SF] =
    oneSf ~ ";" ~ sf ^^ {case f1~_~f2 => SeqSF(f1,f2)} |
    oneSf ~ opt(sf) ^^ {
      case f1~Some(f2) => ParSF(f1,f2)
      case f1~None => f1
    }

  //group expression
  def grExp:Parser[SF] =
    "{" ~> sf <~ "}" ^^ {case f => GExpr(f)}

  /**
    * Parses a simple stream function: match, build or a fun name
    * @return a stream function
    */
  def oneSf:Parser[SF] =
    grExp |
    fbuild |
    fmatch |
    id ~ opt(tparams) ~ opt("<"~> groundParams <~ ">") ^^ {
      case i ~ tps ~ gps => FName(i,tps.getOrElse(List()),gps.getOrElse(List()))
    }

  def fbuild: Parser[SF] =
    "build" ~ opt("<" ~> tname <~ ">") ^^ {case _~ps => FBuild(ps)}

  def fmatch:Parser[SF] =
    "match" ~ opt("<" ~> tname <~ ">") ^^ {case _~ps => FMatch(ps)}

  /* Ground terms */

  /**
    * Parses a ground term: either a constructor or a variable
    * todo: could check if the constructor exists, otherwise error
    * @return a ground term
    */
  def groundTerm:Parser[GT] =
    id ~ opt("(" ~> groundParams <~ ")")  ^^ {
      case n~Some(ps) => IdArgs(n,ps)
      case n~None => if (!sym(n).contains(CONST)) sym=sym.add(n,VAR); Id(n) // if n is not a constructor, add it to the context
    }

  /**
    * Parses parameters which can be ground terms only
    * @return list of ground terms
    */
  def groundParams:Parser[List[GT]] =
    groundTerm ~ rep("," ~> groundTerm) ^^ {case g~gs => g::gs}
}
