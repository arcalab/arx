package dsl

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers


/**
  * Created by guillecledou on 2019-05-31
  */


object Parser extends RegexParsers {

  def parse(program:String):ParseResult[AST] = parseAll(typeDecl,program)

  override def skipWhitespace = true
  override val whiteSpace: Regex = "( |\t|\r|\f|\n|//.*)+".r

  val typeId:Parser[String] = """[A-Z][a-zA-Z0-9_]*""".r
  val pTypeId:Parser[String] = """[a-z][a-zA-Z0-9_]*""".r //possible can just be represeted as a regular typename

  /* Program */


  /* Type declaration */

  // data TypeName = Variants
  // data TypeName<ParamTypes> = Variants

  def typeDecl:Parser[AST] =
    "data" ~
      typeNameDecl ~ "=" ~
        typeVariants ^^ {case _~n~_~vs => TypeDecl(n,vs)}

  def typeNames:Parser[List[AST]] =
    typeName ~ "," ~ typeNames ^^ {case n~_~ns => n::ns} |
    typeName ^^ {case n => List(n)}

  def typeNameDecl:Parser[TypeName] =
    typeId ~ opt("<"~> typeParams <~">")  ^^ {case n~opt =>  TypeName(n,opt.getOrElse(List[AST]()))}

  def typeName:Parser[AST] =
    typeId ~ opt("<"~> typeParams <~">")  ^^ {case n~opt =>  TypeName(n,opt.getOrElse(List[AST]()))} |
    pTypeId ^^ {case n => PTypeName(n)}

  def typeParams:Parser[List[AST]] =
//    ((typeId | pTypeId) ~ ",").* ^^ {case list if list.nonEmpty => }
    typeId ~ "," ~ typeParams ^^ {case n~_~par =>  TypeName(n)::par} |
    pTypeId ~ "," ~ typeParams ^^ {case n~_~par =>  PTypeName(n)::par} |
    typeId  ^^ {case n => List(TypeName(n))} |
    pTypeId ^^ {case n => List(PTypeName(n))}

  def typeVariants: Parser[List[AST]] =
    typeVariant ~ "|" ~ typeVariants ^^ {case v~_~vs => v::vs} |
    typeVariant ^^ {case v => List(v)}

  def typeVariant: Parser[AST] =
    typeId ~ "(" ~ typeNames ~ ")" ^^ {case n~_~params~_ => TypeCons(n,params)}|
    typeId ^^ {case n => TypeVal(n)}

//  def constParam: Parser[List[AST]] =
//    typeId ~ opt("<"~> typeParams <~">") ~"," ~ constParam ^^ {case n~opt~_~par =>  TypeName(n,opt.getOrElse(List()))::par} |
//    pTypeId ~ "," ~ constParam ^^ {case n~_~par =>  PTypeName(n)::par} |
//    typeId  ^^ {case n => List(TypeName(n))} |
//    pTypeId ^^ {case n => List(PTypeName(n))}

}
