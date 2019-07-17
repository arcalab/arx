package dsl.analysis.syntax

import dsl.analysis.syntax.SymbolType._
import dsl.analysis.syntax.ast._
import dsl.common.ParsingException

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

/**
  * Created by guillecledou on 2019-07-16
  */

object NewParser extends RegexParsers with preo.lang.Parser {
  private var adts:List[TypeDeclaration] = List()
  private var conns:List[ConnDef] = List()
  private var sym: SymbolsTable = new SymbolsTable

  def parseProgram(code:String):ParseResult[Program] = {
    adts = List()
    conns = List()
    sym = new SymbolsTable
    parseAll(program,code)
  }

  override def skipWhitespace = true
  override val whiteSpace: Regex = "( |\t|\r|\f|\n|//.*)+".r

  val capitalId:Parser[String] = """[A-Z][a-zA-Z0-9_]*""".r
  val absTypeId:Parser[String] = """[a-z][a-zA-Z0-9_]*""".r //possible can just be represented as a regular typename
  val id:Parser[String] = """[a-zA-Z][a-zA-Z0-9_]*""".r

  /* Program */

  def program:Parser[Program] =
    rep(typeDecl) ~
    rep(funDef) ~
    rep(streamExpr) ^^ { case tds~fun~ses => Program(tds, fun, ses) }

  /* Type declaration */

  def typeDecl:Parser[TypeDeclaration] =
    "data" ~ typeNameDecl ~ "=" ~ typeVariants ^^
      { case _~n~_~vs =>
        adts++=List(TypeDeclaration(n,vs))
        TypeDeclaration(n,vs)
      }

  // Name of the type being declared
  def typeNameDecl:Parser[TypeName] =
    capitalId ~ opt("<"~> typeParams <~">")  ^^ {
      case n~opt => sym=sym.add(n,TYPENAME); ConTypeName(n,opt.getOrElse(List()))
    }

  // Reference to type names abstract or concrete
  def typeNames:Parser[List[TypeName]] =
    typeName ~ "," ~ typeNames ^^ {case n~_~ns => n::ns} |
      typeName ^^ {case n => List(n)}

  def typeName:Parser[TypeName] =
    capitalId ~ opt("<"~> typeParams <~">")  ^^ {case n~opt =>  ConTypeName(n,opt.getOrElse(List()))} |
      absTypeId ^^ {case n => AbsTypeName(n)}

  // Type parameters (abstract or concrete)
  def typeParams:Parser[List[TypeName]] =
    capitalId ~ opt("," ~> typeParams) ^^ {case n~par =>  ConTypeName(n)::par.getOrElse(List())} |
      absTypeId ~ opt("," ~> typeParams) ^^ {case n~par =>  AbsTypeName(n)::par.getOrElse(List())}

  // a list of type variants
  def typeVariants: Parser[List[Variant]] =
    typeVariant ~ opt("|" ~> typeVariants) ^^ {case v~vs => v::vs.getOrElse(List())}

  // a type variant, either value or constructor
  def typeVariant: Parser[Variant] =
    capitalId ~ "(" ~ typeNames ~ ")" ^^
      { case n~_~params~_ => sym=sym.add(n,ADTCONST);/*names::=n*/; AdtConst(n,params)} |
      capitalId ^^
        {case n => sym=sym.add(n,ADTCONST);/*names::=n*/; AdtConst(n)}

  /* Connector definitions */

//  def connDef:Parser[ConnDef] =
//    "def" ~ id ~ "="~"{" ~ preo ~ "}" ^^ {
//      case _~id~_~_~conn~_ => sym=sym.add(id,CONNNAME); conns::=ConnDef(id,conn); ConnDef(id,conn)
//    }

  /* Function definitions */
  def funDef: Parser[FunDefinition] =
    "def" ~ id ~ opt("<" ~> typeParams <~ ">") ~ restFunDef ^^ {
      case _~f~tps~r => r match {
        case Right(sf) => FunSFDef(f,tps.getOrElse(List()),sf)
        case Left((ps,se)) => FunSEDef(f,tps.getOrElse(List()),ps,se)}}

  def restFunDef:Parser[Either[(List[Variable],StreamExpr),StreamFun]] =
    "(" ~ opt(funFormalParams) ~ ")" ~ "=" ~"{"~ streamExpr ~ "}" ^^ { case _~ps~_~_~_~e~_ => Left((ps.getOrElse(List()),e))} |
    "=" ~ "{" ~ streamFun ~ "}" ^^ {case _~_~sf~_ => Right(sf)}

//  def funParams:Parser[Option[List[Variable]]] =


  // comma separated list of identifiers
  def funFormalParams: Parser[List[Variable]] =
    id ~ rep(","~> id) ^^ {
      case id~ids => (id::ids).map(i => {sym=sym.add(i,VARNAME); Variable(i)})}

  /* Assignments */

  /**
    * An assignment expression
    * id = dataExpr |
    * id = connExpr |
    * idList = connExpr
    *
    * @return the abstract syntax tree for the assignment expression
    */
  //def assignment:Parser[AST] =
  //identifierCapOrSmall ~ "=" ~ dataExpr ^^ {case i~_~expr => sym=sym.add(i,VARNAME); Assignment(Identifier(i),expr)}
  def assignment:Parser[StreamExpr] =
    id ~ rep("," ~> id) ~ ":=" ~ assigExpr ~ "." ^^ {
      case i~Nil~_~e~_ => sym=sym.add(i,VARNAME); Assig(List(Variable(i)),e)
      case i~ids~_~e~_ =>
        (i::ids).foreach(i => sym=sym.add(i,VARNAME))
        Assig((i::ids).map(Variable),e)
    }

  /* Expressions */

  def assigExpr:Parser[StreamExpr] =
    simpleAssigExpr ~ assigExpr ^^ {case e1~e2 => ParExpr(e1,e2)} |
    simpleAssigExpr

  def simpleAssigExpr:Parser[StreamExpr] =
    id ~ groundParams ^^ {
      case n~ps if sym.contains(n) && sym(n).get == ADTCONST => ConstExpr(n,ps)
      case n~ps  => FunExpr(FunName(n),ps)
    } |
    id ^^ (n => sym(n) match {
      case Some(ADTCONST) => ConstExpr(n)
      case _ => Variable(n)
    })

  def streamExpr:Parser[StreamExpr] =
    simpleStreamExpr ~ streamExpr ^^ {case e1~e2 => ParExpr(e1,e2)} |
    simpleStreamExpr

  def simpleStreamExpr:Parser[StreamExpr] =
    assignment |
    id ~ groundParams ^^ {
      case n~ps if sym.contains(n) && sym(n).get == ADTCONST => ConstExpr(n,ps)
      case n~ps  => FunExpr(FunName(n),ps)
    } |
    streamFun ~ groundParams ^^ {case sf~ps => FunExpr(sf,ps)} |
    id ^^ (n => sym(n) match {
      case Some(ADTCONST) => ConstExpr(n)
      case _ => Variable(n)
    })

  def streamFun:Parser[StreamFun] =
    simpleStreamFun ~ ";" ~ streamFun ^^ {case sf1~_~sf2 => SeqFun(sf1,sf2)} |
    simpleStreamFun ~ streamFun ^^ {case sf1~sf2 => ParFun(sf1,sf2)}|
    simpleStreamFun

  def simpleStreamFun:Parser[StreamFun] =
    "build" ~ opt("<"~> typeParams <~">") ^^ {case _~ps => Build(ps.getOrElse(List()))} |
    "match" ~ opt("<"~> typeName <~">") ~ "{" ~ rep(option) ~ "}" ^^ {
      case _~Some(ps)~_~os~_ => Match(List(ps),os)
      case _~None~_~os~_ => Match(List(),os)} |
    id ^^ (n => FunName(n))

  def groundTerm:Parser[GroundTerm] =
    capitalId ~ groundParams  ^^ { case n~ps => ConstExpr(n,ps)} |
    id ^^ (i => Variable(i))

  def groundParams:Parser[List[GroundTerm]] =
    "(" ~ groundTerm ~ rep("," ~> groundTerm) ~ ")" ^^ {case _~g~gs~_ => g::gs}

  def option:Parser[Opt] =
    pattern ~ "=>" ~ streamExpr ~ "." ^^ {case p~_~e~_ => Opt(p,e)}

  def pattern:Parser[Pattern] =
    "_" ^^ (_ => Wildcard) |
    groundTerm ^^ (g => GroundPattern(g))
//  /**
//    * A data expression
//    * identifier |
//    * identifier(paramExprs)
//    * @return the parsed expression
//    */
//  def dataExpr:Parser[Expr] =
//    id ~ opt("("~>paramExprs<~")") ^^ {
//      case c ~ None => sym(c) match {
//        case Some(ADTVAL) => AdtTerm(c)
//        case Some(VARNAME) => Identifier(c)
//        case Some(ADTCONST) => throw new ParsingException(s"Missing actual parameters for constructor $c")
//        case Some(CONNNAME) => ConnId(c)
//        case None => Identifier(c)}
//      case c ~ Some(ps) => sym(c) match {
//        case Some(ADTCONST) =>
//          var nparams = sizeOfParams(c)
//          if (nparams == ps.size) AdtConsExpr(c, ps)
//          else throw new ParsingException(s"Constructor $c expected $nparams parameters, but ${ps.size} found")
//        case Some(CONNNAME) =>
//          ConnId(c, ps)
//      }
//    }

  /**
    * Expressions that can appear in actual parameters
    * dataExpr |
    * dataExpr, dataExpr+
    * @return a list containing an expression for each parameter found
    */
//  def paramExprs:Parser[List[Expr]] =
//    dataExpr ~ rep("," ~> dataExpr) ^^ { case e1~e2 => e1::e2 }

  /* Auxiliary functions */

  /**
    * Number of parameters for a given ADT constructor
    * @param adtConst the name of an adt constructor
    * @return
    */
//  private def sizeOfParams(adtConst:String):Int = {
//    var variant =  adts.flatMap(t => t.variants).find(v => v.name == adtConst)
//    if (variant.isDefined)
//      variant.get match {
//        case AdtVal(n) => throw new ParsingException("An ADT Value Variant has no Parameters: ")
//        case AdtConst(n,p) => p.size
//      }
//    else
//      throw new ParsingException("Unknown Constructor: " + adtConst)
//  }
}
