package dsl.analysis.syntax

import dsl.common.ParsingException

import dsl.analysis.syntax.SymType._
import dsl.analysis.syntax.ast._
import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

/**
  * Created by guillecledou on 2019-05-31
  */


object OldParser extends RegexParsers with preo.lang.Parser {

  private var adts:List[TypeDecl] = List()
  private var conns:List[ConnDef] = List()
  private var sym: SymbolsTable = new SymbolsTable

  def parseProgram(code:String):ParseResult[AST] = {
    adts = List()
    conns = List()
    sym = new SymbolsTable
    parseAll(program,code)
  }

  override def skipWhitespace = true
  override val whiteSpace: Regex = "( |\t|\r|\f|\n|//.*)+".r

  val typeId:Parser[String] = """[A-Z][a-zA-Z0-9_]*""".r
  val absTypeId:Parser[String] = """[a-z][a-zA-Z0-9_]*""".r //possible can just be represented as a regular typename
  val identifierCapOrSmall:Parser[String] = """[a-zA-Z][a-zA-Z0-9_]*""".r

  /* Program */

  def program:Parser[AST] =
    rep(typeDecl) ~
      rep(connDef) ~
      rep(funDef) ~
      rep(assignment) ^^ {
      case tds~conn~fun~asg => Statements(tds ++ conn ++ fun ++ asg)
    }

  /* Type declaration */

  def typeDecl:Parser[AST] =
    "data" ~ typeNameDecl ~ "=" ~ typeVariants ^^
      { case _~n~_~vs =>
        adts++=List(TypeDecl(n,vs))
        TypeDecl(n,vs)
      }

  // Name of the type being declared
  def typeNameDecl:Parser[TypeName] =
    typeId ~ opt("<"~> typeParams <~">")  ^^ {
      case n~opt => sym=sym.add(n,TYPE); ConTN(n,opt.getOrElse(List()))
    }

  // Reference to type names abstract or concrete
  def typeNames:Parser[List[TypeName]] =
    typeName ~ "," ~ typeNames ^^ {case n~_~ns => n::ns} |
      typeName ^^ {case n => List(n)}

  def typeName:Parser[TypeName] =
    typeId ~ opt("<"~> typeParams <~">")  ^^ {case n~opt =>  ConTN(n,opt.getOrElse(List()))} |
      absTypeId ^^ {case n => AbsTN(n)}

  // Type parameters (abstract or concrete)
  def typeParams:Parser[List[TypeName]] =
    typeId ~ opt("," ~> typeParams) ^^ {case n~par =>  ConTN(n)::par.getOrElse(List())} |
      absTypeId ~ opt("," ~> typeParams) ^^ {case n~par =>  AbsTN(n)::par.getOrElse(List())}

  // a list of type variants
  def typeVariants: Parser[List[Variant]] =
    typeVariant ~ opt("|" ~> typeVariants) ^^ {case v~vs => v::vs.getOrElse(List())}

  // a type variant, either value or constructor
  def typeVariant: Parser[Variant] =
    typeId ~ "(" ~ typeNames ~ ")" ^^
      { case n~_~params~_ => sym=sym.add(n,CONST);/*names::=n*/; AdtConst(n,params)} |
      typeId ^^
        {case n => sym=sym.add(n,CONST);/*names::=n*/; AdtVal(n)}

  /* Connector definitions */

  def connDef:Parser[ConnDef] =
    "def" ~ identifierCapOrSmall ~ "="~"{" ~ preo ~ "}" ^^ {
      case _~id~_~_~conn~_ => /*sym=sym.add(id,CONNNAME)*/; conns::=ConnDef(id,conn); ConnDef(id,conn)
    }

  /* Function definitions */
  def funDef: Parser[FunDef] =
    "fun" ~ identifierCapOrSmall ~ opt("(" ~> funFormalParams <~ ")") ~ "=" ~ "{" ~ dataExpr ~ "}" ^^ {
      case _~f~params~_~_~e~_ => FunDef(f,e,List(),params.getOrElse(List()))}

  // comma separated list of identifiers
  def funFormalParams: Parser[List[Identifier]] =
    identifierCapOrSmall ~ rep(","~> identifierCapOrSmall) ^^ {
      case id~ids => (id::ids).map(i => {sym=sym.add(i,VAR); Identifier(i)})}

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
  def assignment:Parser[AST] =
    identifierCapOrSmall ~ rep("," ~> identifierCapOrSmall) ~ "=" ~ dataExpr ^^ {
      case i~Nil~_~expr => sym=sym.add(i,VAR); Assignment(List(Identifier(i)),expr)
      case i~ids~_~ConnId(c,ps) =>
        (i::ids).foreach(i => sym=sym.add(i,VAR))
        // make the multiple assignment
        Assignment((i::ids).map(Identifier),ConnId(c,ps))//MultAssignment((i::ids).map(Identifier),ConnId(c,ps))
    }

  /* Expressions */

  /**
    * A data expression
    * identifier |
    * identifier(paramExprs)
    * @return the parsed expression
    */
  def dataExpr:Parser[Expr] =
    identifierCapOrSmall ~ opt("("~>paramExprs<~")") ^^ {
      case c ~ None => sym(c) match {
        case Some(CONST) => AdtTerm(c)
        case Some(VAR) => Identifier(c)
        case Some(CONST) => throw new ParsingException(s"Missing actual parameters for constructor $c")
//        case Some(CONNNAME) => ConnId(c)
        case None => Identifier(c)}
      case c ~ Some(ps) => sym(c) match {
        case Some(CONST) =>
          var nparams = sizeOfParams(c)
          if (nparams == ps.size) AdtConsExpr(c, ps)
          else throw new ParsingException(s"Constructor $c expected $nparams parameters, but ${ps.size} found")
//        case Some(CONNNAME) =>
//          ConnId(c, ps)
      }
    }

  /**
    * Expressions that can appear in actual parameters
    * dataExpr |
    * dataExpr, dataExpr+
    * @return a list containing an expression for each parameter found
    */
  def paramExprs:Parser[List[Expr]] =
    dataExpr ~ rep("," ~> dataExpr) ^^ { case e1~e2 => e1::e2 }

  /* Auxiliary functions */

  /**
    * Number of parameters for a given ADT constructor
    * @param adtConst the name of an adt constructor
    * @return
    */
  private def sizeOfParams(adtConst:String):Int = {
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
