package dsl.analysis.syntax

import dsl.common.ParsingException
import preo.ast.Connector
import preo.{DSL, lang}

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers
import dsl.analysis.syntax.SymbolType._

/**
  * Created by guillecledou on 2019-05-31
  */

object Parser extends RegexParsers with preo.lang.Parser {

  private var adts:List[TypeDecl] = List()
  private var conns:List[ConnDef] = List()
//  private var names:List[String] = List() // no need for scopes for now.
  private var sym: SymbolsTable = new SymbolsTable

  def parseProgram(code:String):ParseResult[AST] = {
    adts = List()
    conns = List()
//    names = List()
    sym = new SymbolsTable
    parseAll(program,code)
  }

  override def skipWhitespace = true
  override val whiteSpace: Regex = "( |\t|\r|\f|\n|//.*)+".r

  val typeId:Parser[String] = """[A-Z][a-zA-Z0-9_]*""".r
  val parametricTypeId:Parser[String] = """[a-z][a-zA-Z0-9_]*""".r //possible can just be represented as a regular typename
  val identifierCapOrSmall:Parser[String] = """[a-zA-Z][a-zA-Z0-9_]*""".r

  /* Program */

  def program:Parser[AST] =
    opt(rep(typeDecl)) ~
      opt(rep(connDef)) ~
      opt(rep(assignment)) ^^ {
        case tds~conn~asg => Statements(tds.getOrElse(List()) ++ conn.getOrElse(List()) ++asg.getOrElse(List()))
      }

  /* Type declaration */

  def typeDecl:Parser[AST] =
    "data" ~ typeNameDecl ~ "=" ~ typeVariants ^^
      { case _~n~_~vs =>
          adts++=List(TypeDecl(n,vs))
          TypeDecl(n,vs)
        }

  def typeNameDecl:Parser[TypeName] =
    typeId ~ opt("<"~> typeParams <~">")  ^^ {
      case n~opt => sym=sym.add(n,TYPENAME); ConTypeName(n,opt.getOrElse(List()))
    }

//  def typeNameDecl:Parser[TypeName] =
//    typeId ~ opt("<"~> typeParams <~">")  ^^ {
//      case n~opt if names.contains(n) => throw new ParsingException(s"Name $n is already declared within the scope")
//      case n~opt =>  names::=n; ConTypeName(n,opt.getOrElse(List()))
//    }

  def typeNames:Parser[List[TypeName]] =
    typeName ~ "," ~ typeNames ^^ {case n~_~ns => n::ns} |
      typeName ^^ {case n => List(n)}

  def typeName:Parser[TypeName] =
    typeId ~ opt("<"~> typeParams <~">")  ^^ {case n~opt =>  ConTypeName(n,opt.getOrElse(List()))} |
    parametricTypeId ^^ {case n => AbsTypeName(n)}

  def typeParams:Parser[List[TypeName]] =
    typeId ~ "," ~ typeParams ^^ {case n~_~par =>  ConTypeName(n)::par} |
    parametricTypeId ~ "," ~ typeParams ^^ {case n~_~par =>  AbsTypeName(n)::par} |
    typeId  ^^ {case n => List(ConTypeName(n))} |
    parametricTypeId ^^ {case n => List(AbsTypeName(n))}

  def typeVariants: Parser[List[Variant]] =
    typeVariant ~ opt("|" ~> typeVariants) ^^ {case v~vs => v::vs.getOrElse(List())}

  def typeVariant: Parser[Variant] =
    typeId ~ "(" ~ typeNames ~ ")" ^^
      { case n~_~params~_ => sym=sym.add(n,ADTCONST);/*names::=n*/; AdtConst(n,params)} |
    typeId ^^
      {case n => sym=sym.add(n,ADTVAL);/*names::=n*/; AdtVal(n)}

//  def typeVariant: Parser[Variant] =
//    typeId ~ "(" ~ typeNames ~ ")" ^^ {
//      case n~_~params~_ if names.contains(n) => throw new ParsingException(s"Name $n is already declared within the scope")
//      case n~_~params~_ => names::=n; AdtConst(n,params)
//    } |
//      typeId ^^ {
//        case n if names.contains(n) => throw new ParsingException(s"Name $n is already declared within the scope")
//        case n => names::=n; AdtVal(n)
//      }

  /* Connector definitions */

  def connDef:Parser[ConnDef] =
  "def" ~ identifierCapOrSmall ~ "="~"{" ~ preo ~ "}" ^^ {
    case _~id~_~_~conn~_ => sym=sym.add(id,CONNNAME); conns::=ConnDef(id,conn); ConnDef(id,conn)
  }

  /* Assignments */

  def assignment:Parser[AST] =
    identifierCapOrSmall ~ "=" ~ dataExpr ^^ {case i~_~expr => sym=sym.add(i,VARNAME); Assignment(Identifier(i),expr)}

  /* Expressions */

  /* Data Expressions */

  def dataExpr:Parser[Expr] =
    identifierCapOrSmall ~ "("~paramExprs~")" ^^ {
      case c~_~par~_ if sym(c)==ADTCONST =>
        if (sizeOfParams(c) == par.size)
          AdtConsExpr(c,par)
        else throw new ParsingException("Number of actual parameters does not corresponds with number of formal " +
            "parameters for constructor: "+ c)
      case c~_~par~_ if sym(c)==CONNNAME =>
        // todo: check number of input parameters or total parameters
        ConnId(c,par)
      } |
    identifierCapOrSmall ^^ {
      case i if sym(i) == ADTVAL   => AdtTerm(i)
      case i if sym(i) == VARNAME  => Identifier(i)
      case i if sym(i) == ADTCONST => throw new ParsingException(s"Missing actual parameters for constructor ${i}")
      case i if sym(i) == CONNNAME => ConnId(i)
    }

//  def dataExpr:Parser[Expr] =
//    identifierCapOrSmall ~ "("~paramExprs~")" ^^ {
//      case c~_~par~_ if (c.matches(adtConstructor)) =>
//        if (sizeOfParams(c) == par.size)
//          AdtConsExpr(c,par)
//        else
//          throw new ParsingException("Number of actual parameters does not corresponds with number of formal " +
//            "parameters for constructor: "+ c)
//      case c~_~par~_ => throw new ParsingException("Unknown Constructor: " + c )} |
//      identifierCapOrSmall ^^ {case i => if(i.matches(adtValue)) AdtTerm(i) else Identifier(i) }

  def paramExprs:Parser[List[Expr]] =
    dataExpr ~ "," ~ dataExpr ^^ {case e1~_~e2 => List(e1,e2)} |
      dataExpr ^^ {case e => List(e)}

  /* Conn Expressions */



  /* ADT Auxiliary functions */

//  /**
//    * regex expression containing all known adt constructor names
//    * @return
//    */
//  lazy val adtConstructor:String = adts.flatMap(t => t.variants).map(v => v match {
//      case AdtVal(n) => ""
//      case AdtConst(n,param) => n.r
//    }).filterNot(_ == "").mkString("|")
//
//  /**
//    * regex expression containing all known adt value names
//    * @return
//    */
//  lazy val adtValue:String = adts.flatMap(t => t.variants).map(v => v match {
//      case AdtVal(n) => n.r
//      case AdtConst(n,param) => ""
//    }).filterNot(_ == "").mkString("|")

  /**
    * Number of parameters for a given ADT constructor
    * @param adtConst
    * @return
    */
  def sizeOfParams(adtConst:String):Int = {
    var variant =  adts.flatMap(t => t.variants).find(v => v.name == adtConst)
    if (variant.isDefined)
      variant.get match {
       case AdtVal(n) => throw new ParsingException("An ADT Value Variant has no Parameters: ")
       case AdtConst(n,p) => p.size
      }
    else
      throw new ParsingException("Unknown Constructor: " + adtConst)
  }

}
