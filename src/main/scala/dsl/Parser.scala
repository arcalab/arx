package dsl

import common.ParsingException

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
  val pTypeId:Parser[String] = """[a-z][a-zA-Z0-9_]*""".r //possible can just be represented as a regular typename
  val identifier:Parser[String] = """[a-zA-Z][a-zA-Z0-9_]*""".r
  /* Program */

  def program:Parser[AST] =
    opt(rep(typeDecl)) ~
      opt(rep(assignment)) ^^ {
        case tds~asg => Statements(tds.getOrElse(List()) ++ asg.getOrElse(List()))
      }

  /* Type declaration */

  // data TypeName = Variants
  // data TypeName<ParamTypes> = Variants

  def typeDecl:Parser[AST] =
    "data" ~
      typeNameDecl ~ "=" ~
        typeVariants ^^ {case _~n~_~vs =>
          adts++=List(TypeDecl(n,vs))
          TypeDecl(n,vs)
        }

  def typeNameDecl:Parser[TypeName] =
    typeId ~ opt("<"~> typeParams <~">")  ^^ {case n~opt =>  TypeName(n,opt.getOrElse(List[AST]()))}

  def typeNames:Parser[List[AST]] =
    typeName ~ "," ~ typeNames ^^ {case n~_~ns => n::ns} |
      typeName ^^ {case n => List(n)}

  def typeName:Parser[AST] =
    typeId ~ opt("<"~> typeParams <~">")  ^^ {case n~opt =>  TypeName(n,opt.getOrElse(List[AST]()))} |
    pTypeId ^^ {case n => PTypeName(n)}

  def typeParams:Parser[List[AST]] =
    typeId ~ "," ~ typeParams ^^ {case n~_~par =>  TypeName(n)::par} |
    pTypeId ~ "," ~ typeParams ^^ {case n~_~par =>  PTypeName(n)::par} |
    typeId  ^^ {case n => List(TypeName(n))} |
    pTypeId ^^ {case n => List(PTypeName(n))}

  def typeVariants: Parser[List[AST]] =
//    typeVariant ~ "|" ~ typeVariants ^^ {case v~_~vs =>  v::vs} |
    typeVariant ~ opt("|" ~> typeVariants) ^^ {case v~vs => v::vs.getOrElse(List()).toList} //|
//    typeVariant ^^ {case v => List(v)}

  def typeVariant: Parser[AST] =
    typeId ~ "(" ~ typeNames ~ ")" ^^ {case n~_~params~_ => TypeCons(n,params)}|
    typeId ^^ {case n => TypeVal(n)}

//  def constParam: Parser[List[AST]] =
//    typeId ~ opt("<"~> typeParams <~">") ~"," ~ constParam ^^ {case n~opt~_~par =>  TypeName(n,opt.getOrElse(List()))::par} |
//    pTypeId ~ "," ~ constParam ^^ {case n~_~par =>  PTypeName(n)::par} |
//    typeId  ^^ {case n => List(TypeName(n))} |
//    pTypeId ^^ {case n => List(PTypeName(n))}


  /* Assignments */

  def assignment:Parser[AST] =
    identifier ~ "=" ~ expr ^^ {case i~_~expr => Assignment(Identifier(i),expr)}

  /* Expressions */

  def expr:Parser[AST] =
    identifier ~ "("~paramExprs~")" ^^ {
      case c~_~par~_ if (c.matches(adtConstructor)) => AdtConsExpr(c,par)
      case c~_~par~_ => throw new ParsingException("Unknown Constructor: " + c )} |
    identifier ^^ {case i => if(i.matches(adtValue)) AdtTerm(i) else Identifier(i) }

  def paramExprs:Parser[List[AST]] =
    expr ~ "," ~ expr ^^ {case e1~_~e2 => List(e1,e2)} |
      expr ^^ {case e => List(e)}

  /* ADT Expressions */

//  def adtExpr:Parser[AST] =
//    adtConstructor ~ "("~paramExprs~")" ^^ {case c~_~par~_ => AdtConsExpr(c,par) } |
//    adtValue ^^ {case n => AdtTerm(n)} |
//    identifier ^^ {case n => Identifier(n)}

//  def paramExprs:Parser[List[AST]] =
//    adtExpr ~ "," ~ adtExpr ^^ {case e1~_~e2 => List(e1,e2)} |
//      adtExpr ^^ {case e => List(e)}
//
  def adtConstructor:String = {//Parser[String] ={
    adts.flatMap(t => t.variants).map(v => v match {
      case TypeVal(n) => ""
      case TypeCons(n,param) => n.r
    }).filterNot(_ == "").mkString("|")
  }

  def adtValue:String = {//:Parser[String] = {
    adts.flatMap(t => t.variants).map(v => v match {
      case TypeVal(n) => n.r
      case TypeCons(n,param) => ""
    }).filterNot(_ == "").mkString("|")
  }//.foldRight("")(_+"|"+_).r



  /* Functions */



}
