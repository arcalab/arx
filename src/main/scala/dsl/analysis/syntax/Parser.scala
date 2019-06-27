package dsl.analysis.syntax

import dsl.DSL
import dsl.common.ParsingException
import preo.ast.Connector
import preo.{DSL, lang}

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers
import dsl.analysis.syntax.SymbolType._
import dsl.backend.Show
import preo.backend.Network

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
    rep(typeDecl) ~
      rep(connDef) ~
      rep(assignment) ^^ {
        case tds~conn~asg => Statements(tds ++ conn ++ asg)
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
      case n~opt => sym=sym.add(n,TYPENAME); ConTypeName(n,opt.getOrElse(List()))
    }

  // Reference to type names abstract or concrete
  def typeNames:Parser[List[TypeName]] =
    typeName ~ "," ~ typeNames ^^ {case n~_~ns => n::ns} |
      typeName ^^ {case n => List(n)}

  def typeName:Parser[TypeName] =
    typeId ~ opt("<"~> typeParams <~">")  ^^ {case n~opt =>  ConTypeName(n,opt.getOrElse(List()))} |
    parametricTypeId ^^ {case n => AbsTypeName(n)}

  // Type parameters (abstract or concrete)
  def typeParams:Parser[List[TypeName]] =
    typeId ~ opt("," ~> typeParams) ^^ {case n~par =>  ConTypeName(n)::par.getOrElse(List())} |
    parametricTypeId ~ opt("," ~> typeParams) ^^ {case n~par =>  AbsTypeName(n)::par.getOrElse(List())}

  // a list of type variants
  def typeVariants: Parser[List[Variant]] =
    typeVariant ~ opt("|" ~> typeVariants) ^^ {case v~vs => v::vs.getOrElse(List())}

  // a type variant, either value or constructor
  def typeVariant: Parser[Variant] =
    typeId ~ "(" ~ typeNames ~ ")" ^^
      { case n~_~params~_ => sym=sym.add(n,ADTCONST);/*names::=n*/; AdtConst(n,params)} |
    typeId ^^
      {case n => sym=sym.add(n,ADTVAL);/*names::=n*/; AdtVal(n)}

  /* Connector definitions */

  def connDef:Parser[ConnDef] =
  "def" ~ identifierCapOrSmall ~ "="~"{" ~ preo ~ "}" ^^ {
    case _~id~_~_~conn~_ => sym=sym.add(id,CONNNAME); conns::=ConnDef(id,conn); ConnDef(id,conn)
  }

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
        case i~Nil~_~expr => sym=sym.add(i,VARNAME); Assignment(Identifier(i),expr)
        case i~ids~_~ConnId(c,ps) =>
          (i::ids).foreach(i => sym=sym.add(i,VARNAME))
          // find the definition of the conector with name c
          val cdef = conns.find(_.name==c).get
          // reduce the connector and create a network to find inputs and outputs of c
          val net = Network(dsl.DSL.unsafeCoreConnector(cdef.c))
          // make the multiple assignment
          var res = MultAssignment((i::ids).map(Identifier),ConnId(c,ps))
          // if the number of i::ids corresponds to the number of outputs of c return the result
          if ((i::ids).size == net.outs.size)
            res
            // otherwise error
          else throw new ParsingException(s"The list of variables on the LHS of ${Show(res)} " +
            s"does not correspond with the number of outputs of connector $c")
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
        case ADTVAL => AdtTerm(c)
        case VARNAME => Identifier(c)
        case ADTCONST => throw new ParsingException(s"Missing actual parameters for constructor $c")
        case CONNNAME => ConnId(c)}
      case c ~ Some(ps) => sym(c) match {
        case ADTCONST =>
          var nparams = sizeOfParams(c)
          if (nparams == ps.size) AdtConsExpr(c, ps)
          else throw new ParsingException(s"Constructor $c expected $nparams parameters, but ${ps.size} found")
        case CONNNAME =>
          // todo: check number of input parameters or total parameters
          ConnId(c, ps)}
    }

  /**
    * Expressions that can appear in actual parameters
    * dataExpr |
    * dataExpr, dataExpr+
    * @return a list containing an expression for each parameter found
    */
  def paramExprs:Parser[List[Expr]] =
    dataExpr ~ "," ~ dataExpr ^^ {case e1~_~e2 => List(e1,e2)} |
    dataExpr ^^ {e => List(e)}

  /* Auxiliary functions */

  /**
    * Number of parameters for a given ADT constructor
    * @param adtConst the name of an adt constructor
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
