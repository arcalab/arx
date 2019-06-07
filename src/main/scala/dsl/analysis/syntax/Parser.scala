package dsl.analysis.syntax

import dsl.common.ParsingException

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

/**
  * Created by guillecledou on 2019-05-31
  */


object Parser extends RegexParsers {

  private var adts:List[TypeDecl] = List()

  def parse(code:String):ParseResult[AST] = {
    adts = List()
    parseAll(program,code)
  }

  override def skipWhitespace = true
  override val whiteSpace: Regex = "( |\t|\r|\f|\n|//.*)+".r


  val typeId:Parser[String] = """[A-Z][a-zA-Z0-9_]*""".r
  val parametricTypeId:Parser[String] = """[a-z][a-zA-Z0-9_]*""".r //possible can just be represented as a regular typename
  val identifier:Parser[String] = """[a-zA-Z][a-zA-Z0-9_]*""".r

  /* Program */

  def program:Parser[AST] =
    opt(rep(typeDecl)) ~
      opt(rep(assignment)) ^^ {
        case tds~asg => Statements(tds.getOrElse(List()) ++ asg.getOrElse(List()))
      }

  /* Type declaration */

  def typeDecl:Parser[AST] =
    "data" ~
      typeNameDecl ~ "=" ~
        typeVariants ^^ {case _~n~_~vs =>
          adts++=List(TypeDecl(n,vs))
          TypeDecl(n,vs)
        }

  def typeNameDecl:Parser[TypeName] =
    typeId ~ opt("<"~> typeParams <~">")  ^^ {case n~opt =>  ConTypeName(n,opt.getOrElse(List()))}

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
    typeId ~ "(" ~ typeNames ~ ")" ^^ {case n~_~params~_ => AdtConst(n,params)}|
    typeId ^^ {case n => AdtVal(n)}

  /* Assignments */

  def assignment:Parser[AST] =
    identifier ~ "=" ~ expr ^^ {case i~_~expr => Assignment(Identifier(i),expr)}

  /* Expressions */

  def expr:Parser[Expr] =
    identifier ~ "("~paramExprs~")" ^^ {
      case c~_~par~_ if (c.matches(adtConstructor)) => AdtConsExpr(c,par)
      case c~_~par~_ => throw new ParsingException("Unknown Constructor: " + c )} |
    identifier ^^ {case i => if(i.matches(adtValue)) AdtTerm(i) else Identifier(i) }

  def paramExprs:Parser[List[Expr]] =
    expr ~ "," ~ expr ^^ {case e1~_~e2 => List(e1,e2)} |
      expr ^^ {case e => List(e)}

  /* ADT Expressions */

  def adtConstructor:String = adts.flatMap(t => t.variants).map(v => v match {
      case AdtVal(n) => ""
      case AdtConst(n,param) => n.r
    }).filterNot(_ == "").mkString("|")

  def adtValue:String = adts.flatMap(t => t.variants).map(v => v match {
      case AdtVal(n) => n.r
      case AdtConst(n,param) => ""
    }).filterNot(_ == "").mkString("|")

  /* Functions */
}
