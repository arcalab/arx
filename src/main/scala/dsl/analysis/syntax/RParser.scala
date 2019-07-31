package dsl.analysis.syntax

import dsl.analysis.syntax.SymType._
import dsl.analysis.syntax.ast._


import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

/**
  * Created by guillecledou on 2019-07-16
  */

object RParser extends RegexParsers with preo.lang.Parser {
  private var adts:List[TD] = List()
  private var constr:List[String] = List()
  private var sym: SymbolsTable = new SymbolsTable

  def parseProgram(code:String):ParseResult[NewProg] = {
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

  def program:Parser[NewProg] =
    rep(dt) ~
      //    rep(funDef) ~
      se ^^ { case tds~e => NewProg(tds, e) }

  /* Type declaration */

  // data type declaration
  def dt:Parser[TD] =
    "data" ~ capitalId ~ opt(tparams) ~ "=" ~ constructors ^^
      { case _~n~tps~_~vs => sym=sym.add(n,TYPE);val td = TD(ConTypeName(n,tps.getOrElse(List())),vs) ; adts::=td ; td}
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
  def funDef: Parser[SExpr] =
    "def" ~>  restFunDef

//  def restFunDef:Parser[FDef] = {
//    sym = sym.addLevel()
//    id ~ opt(tparams) ~
//      opt("<" ~> dataFormalParams <~ ">") ~
//      opt("(" ~> opt(funFormalParams) <~ ")") ~
//      "=" ~ "{" ~ se ~ "}" ^^ {
//      case i ~ tps ~ dps ~ Some(ps) ~ _ ~ _ ~ e ~ _ =>
//        sym = sym.rmLevel(); sym = sym.add(i, FUN);FDef(i, tps.getOrElse(List()), dps.getOrElse(List()), e, ps.getOrElse(List()))
//      case i ~ tps ~ dps ~ None ~ _ ~ _ ~ e ~ _ =>
//        sym = sym.rmLevel(); sym = sym.add(i, FUN); FDef(i, tps.getOrElse(List()), dps.getOrElse(List()), e)
//    }
//  }
  def restFunDef:Parser[SExpr] = {
    sym = sym.addLevel()
    id ~ opt(tparams) ~ opt("<" ~> dataFormalParams <~ ">") ~ funParamsAndBody ^^ {
      case f ~ tps ~ dps ~ r => r match {
        case Right(sf) => {
          sym = sym.rmLevel()
          sym = sym.add(f, FUN);
          FFDef(f, tps.getOrElse(List()), dps.getOrElse(List()), sf)
        }
        case Left((ps, se)) => {
          sym = sym.rmLevel()
          sym = sym.add(f, FUN);
          FEDef(f, tps.getOrElse(List()), dps.getOrElse(List()), ps, se)
        }
      }
    }
  }

  def funParamsAndBody:Parser[Either[(List[Id],SExpr),SFun]] =
    "(" ~ opt(funFormalParams) ~ ")" ~ "=" ~"{"~ se ~ "}" ^^ { case _~ps~_~_~_~e~_ => println("found sed");Left((ps.getOrElse(List()),e))} |
      "=" ~ "{" ~ sf ~ "}" ^^ {case _~_~sf~_ => println("found sfd");Right(sf)}

  //  def funParams:Parser[Option[List[Variable]]] =


  def funFormalParams: Parser[List[Id]] =
    id ~ rep(","~> id) ^^ {
      case id~ids => (id::ids).map(i => {sym=sym.add(i,INPUT); Id(i)})}

  def dataFormalParams: Parser[List[Id]] =
    id ~ rep(","~> id) ^^ {
      case id~ids => (id::ids).map(i => {sym=sym.add(i,DATA); Id(i)})}

  /* Assignments */

  def assignment:Parser[SExpr] =
    id ~ rep("," ~> id) ~ ":=" ~ se ~ "." ^^ {
      case i~Nil~_~e~_ => sym=sym.add(i,VAR); Asg(List(Id(i)),e)
      case i~ids~_~e~_ =>
        (i::ids).foreach(i => sym=sym.add(i,VAR))
        Asg((i::ids).map(Id),e)
    }

  /* Stream Expressions */

  def se:Parser[SExpr] =
    oneSe ~ opt(se) ^^ {
      case e1 ~ Some(e2) => ParSExpr(e1, e2)
      case e1 ~ None => e1
    }

  def oneSe:Parser[SExpr] =
    funDef |
    assignment |
    fbuild ~ "("~ groundParams ~ ")" ^^ {case b~_~ps~_ => FExpr(b,ps)} |
    fmatch ~ "("~ groundParams ~ ")" ^^ {case b~_~ps~_ => FExpr(b,ps)} |
    id ~ "("~ groundParams ~ ")" ^^ {case i~_~ps~_ => IdArgs(i,ps)} |
    id ^^ {i => if (!sym(i).contains(CONST)) sym=sym.add(i,VAR); Id(i)} |
    sf ~ "("~ groundParams ~ ")" ^^ {case i~_~ps~_ => FExpr(i,ps)}

//  def simple:Parser[SFun] =
//    fbuild |
//    fmatch |
//    id ^^ (i => Id(i))

  //  def parSomething:Parser[SExpr] =
//    id ~

  /* Stream Functions */


  /**
    * Parses a stream function: sequence of functions, functions in parallel,
    * or a simple stream function
    * @return a stream function
    */
  def sf:Parser[SFun] =
//    grExp |
    oneSf ~ ";" ~ sf ^^ {case f1~_~f2 => SeqSFun(f1,f2)} |
    oneSf ~ opt(sf) ^^ {
      case f1~Some(f2) => ParSFun(f1,f2)
      case f1~None => f1
    }

  def grExp:Parser[SFun] =
    "{" ~> sf <~ "}" ^^ {case f => GExpr(f)}

  /**
    * Parses a simple stream function: match, build or a fun name
    * @return a stream function
    */
  def oneSf:Parser[SFun] =
    grExp|
    fbuild |
    fmatch |
      id ~ opt(tparams) ~ opt("<"~> groundParams <~ ">") ^^ {
//        case i ~ None ~ None => (i)
        case i ~ tps ~ gps => FName(i,tps.getOrElse(List()),gps.getOrElse(List()))
      }
  def fbuild: Parser[SFun] =
    "build" ~ opt("<" ~> tname <~ ">") ^^ {case _~ps => FBuild(ps)}

  def fmatch:Parser[SFun] =
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
      case n~None => sym=sym.add(n,VAR); Id(n)
    }

  /**
    * Parses parameters which can be ground terms only
    * @return list of ground terms
    */
  def groundParams:Parser[List[GT]] =
    groundTerm ~ rep("," ~> groundTerm) ^^ {case g~gs => g::gs}
}
