package dsl.analysis.syntax

import dsl.analysis.syntax.Program.Block
import dsl.analysis.syntax.SymbolType._
import dsl.common.ParsingException

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

/**
  * Created by guillecledou on 2019-05-31
  */

object Parser2 extends RegexParsers {

//  private var adts:List[TypeDecl2] = List()
  //private var conns:List[ConnDef] = List()
//  private var names:List[String] = List() // no need for scopes for now.
  private var sym: SymbolsTable = new SymbolsTable

  def parseProgram(code:String):ParseResult[Program] = {
//    adts = List()
    //conns = List()
    sym = new SymbolsTable
    parseAll(program,code)
  }

  override def skipWhitespace = true
  override val whiteSpace: Regex = "( |\t|\r|\f|\n|//.*)+".r

  val capitalId:Parser[String] = """[A-Z][a-zA-Z0-9_]*""".r
  val lowerCaseId:Parser[String] = """[a-z][a-zA-Z0-9_]*""".r //possible can just be represented as a regular typename
  val id:Parser[String] = """[a-zA-Z][a-zA-Z0-9_]*""".r

  // todo: use it
  val keywords: Set[String] = Set("data","def","match","build")
  // add later predef types "True","False","Left","Right","U","Bool","List","Either","Map","Int","Pair","Unit"

  /* Program */

  def program:Parser[Program] =
    rep(dt) ~ block ^^ {
      case typs~bl => Program(typs,bl)
    }

  def block: Parser[Block] =
    rep(statement)

  def statement: Parser[Statement] =
    funDef | assignment | strExpr

  def strExpr: Parser[StreamExpr] =
    strFun~args ^^ {
      case f~as => FunctionApp(f,as)
    } |
    ground

  def args: Parser[List[GroundTerm]] =
    "("~>repsep(ground,",")<~")"

  def ground: Parser[GroundTerm] =
    lowerCaseId ^^ Port |
    capitalId ~ opt(args) ^^ {
      case q~as => Const(q,as.getOrElse(Nil))
    }

//  def strFun: Parser[StreamFun] =
//    oneStrFun | groupStrFun

  def strFun: Parser[StreamFun] =
    "build" ^^^ Build |
    "match" ^^^ Match |
    groupStrFun
//    lowerCaseId ^^ FunName |
    // TODO: sequential and parallel composition

  //      rep(connDef) ~
//      rep(funDef) ~
//      rep(assignment) ^^ {
//        case tds~conn~fun~asg => Statements(tds ++ conn ++ fun ++ asg)
//      }

  def groupStrFun:Parser[StreamFun] =
    lowerCaseId ^^ FunName |
    "{" ~> parOrSeq <~ "}"


  def parOrSeq:Parser[StreamFun] =
    groupStrFun ~ ";" ~ parOrSeq ^^ {case f1 ~_~f2 => SeqFun(f1,f2)} |
    groupStrFun ~ opt(parOrSeq) ^^ {case f1~f2 => if (f2.isDefined) ParFun(f1,f2.get) else f1}

  /* Type declaration and type parameters */


  def dt:Parser[TypeDecl2] =
    "data" ~ capitalId ~ opt(tparams) ~ "=" ~ constructors ^^ {
      case _~n~tps~_~vs => sym=sym.add(n,TYPE);TypeDecl2(ConTypeName(n,tps.getOrElse(List())),vs)}

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
    capitalId ~ opt(tparams)  ^^ {case n~opt =>  ConTypeName(n,opt.getOrElse(List()))} |
    lowerCaseId ^^ {case n => AbsTypeName(n)}

  // type parameters
  def tparams:Parser[List[TypeName]] =
    "<" ~> tnames <~ ">"

//  def dt:Parser[TypeDecl2] =
//    "data" ~ typeNameDecl ~ "=" ~ typeVariants ^^
//      { case _~n~_~vs =>
//          adts++=List(TypeDecl2(n,vs))
//          TypeDecl2(n,vs)
//        }

//  // Name of the type being declared
//  def typeNameDecl:Parser[TypeName] =
//    capitalId ~ opt("<"~> typeParams <~">")  ^^ {
//      case n~opt => sym=sym.add(n,TYPENAME); ConTypeName(n,opt.getOrElse(List()))
//    }

//  // Reference to type names abstract or concrete
//  def typeNames:Parser[List[TypeName]] =
//    typeName ~ "," ~ typeNames ^^ {case n~_~ns => n::ns} |
//      typeName ^^ (n => List(n))
//
//  def typeName:Parser[TypeName] =
//    typeId ~ opt("<"~> typeParams <~">")  ^^ {case n~opt =>  ConTypeName(n,opt.getOrElse(List()))} |
//    lowerCaseId ^^ (n => AbsTypeName(n))
//
//  // Type parameters (abstract or concrete)
//  def typeParams:Parser[List[TypeName]] =
//    typeId ~ opt("," ~> typeParams) ^^ {case n~par =>  ConTypeName(n)::par.getOrElse(List())} |
//    lowerCaseId ~ opt("," ~> typeParams) ^^ {case n~par =>  AbsTypeName(n)::par.getOrElse(List())}
//
//  // a list of type variants
//  def typeVariants: Parser[List[Constructor]] =
//    typeVariant ~ opt("|" ~> typeVariants) ^^ {case v~vs => v::vs.getOrElse(List())}
//
//  // a type variant, either value or constructor
//  def typeVariant: Parser[Constructor] =
//    typeId ~ "(" ~ typeNames ~ ")" ^^
//      { case n~_~params~_ => sym=sym.add(n,ADTCONST);/*names::=n*/; Constructor(n,params)} |
//    typeId ^^
//      { n => sym = sym.add(n, ADTVAL); /*names::=n*/ ; Constructor(n) }


  /* Connector definitions */

//  def connDef:Parser[ConnDef] =
//    "def" ~ identifierCapOrSmall ~ "="~"{" ~ preo ~ "}" ^^ {
//      case _~id~_~_~conn~_ => sym=sym.add(id,CONNNAME); conns::=ConnDef(id,conn); ConnDef(id,conn)
//  }

  /* Function definitions */

  //  def funDef: Parser[FunDef2] =
  //    "def" ~ lowerCaseId ~ opt("(" ~> funFormalParams <~ ")") ~
  //      opt(":"~>tname)~
  //      "=" ~ "{" ~ block ~ "}" ^^ {
  //      case _~f~params~typ~_~_~bl~_ => FunDef2(f,params.getOrElse(Nil),typ,bl)}


  def funDef: Parser[Statement] =
    "def" ~>  restFunDef

  def restFunDef:Parser[Statement] = {
    sym = sym.addLevel()
    lowerCaseId /* ~ opt("<" ~> dataFormalParams <~ ">")*/ ~ funParamsAndBody ^? ({
      case f ~ rest  if !keywords.contains(f) =>
        sym = sym.add(f, FUN) // quick hack to see if function name is not used/delared inside the function todo: fix it
        sym = sym.rmLevel()
        sym = sym.add(f, FUN) // actually added it to the current scope
        rest match {
          case Right((typ,sf)) => SFunDef(f, typ, sf)
          case Left((ps,typ, bl)) => FunDef2(f,ps, typ, bl)}},
      {case f~_ => sym=sym.rmLevel(); s"Cannot use the reserve word $f as a function name"})
  }

  def funParamsAndBody:Parser[Either[(List[TypedVar],Option[TypeName],Block),(Option[TypeName],StreamFun)]] =
    "(" ~ opt(funFormalParams) ~ ")" ~ opt(":"~>tname) ~ "=" ~ "{" ~ block ~ "}" ^^
      {case _~ps~_~typ~_~_~bl~_ => Left((ps.getOrElse(List()),typ,bl))} |
     opt(":"~>tname) ~ "=" ~ "{" ~ strFun ~ "}" ^^
      {case typ~_~_~sf~_ => Right((typ,sf))}

  // todo: dropped for now until we solve it in the paper
  def dataFormalParams: Parser[List[Port]] =
    id ~ rep(","~> id) ^^ { case id~ids => (id::ids).map(i => {sym=sym.add(i,DATA); Port(i)})}

  // comma separated list of identifiers
  def funFormalParams: Parser[List[TypedVar]] =
    repsep(typedVar,",") ^^ { tvs =>
      tvs.foreach(tv => sym = sym.add(tv.name,INPUT)) //todo: Possibly no need of using INPUT, just use VAR
      tvs
    }

  def typedVar: Parser[TypedVar] =
    lowerCaseId~opt(":"~>tname) ^^ {
      case v~t => TypedVar(v,t)
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
  def assignment:Parser[Assignment2] =
    repsep(lowerCaseId,",")~":="~strExpr ^^ {
      case ids~_~expr =>
        ids.foreach(id => sym = sym.add(id,VAR))
        Assignment2(ids,expr)
    }



//  /* Expressions */
//
//  /**
//    * A data expression
//    * identifier |
//    * identifier(paramExprs)
//    * @return the parsed expression
//    */
//  def dataExpr:Parser[Expr] =
//    lowerCaseId ~ opt("("~>paramExprs<~")") ^^ {
//      case c ~ None => sym(c) match {
//        case Some(ADTVAL) => AdtTerm(c)
//        case Some(VARNAME) => Identifier(c)
//        case Some(ADTCONST) => throw new ParsingException(s"Missing actual parameters for constructor $c")
//        case Some(CONNNAME) => ConnId(c)
//          //TODO: @Guille - some symbols were not captured. Check what you had in mind.
//        case Some(x) => throw new ParsingException(s"Unexpected identifier $c with symbol $x")
//        case None => Identifier(c)}
//      case c ~ Some(ps) => sym(c) match {
//        case Some(ADTCONST) =>
//          var nparams = sizeOfParams(c)
//          if (nparams == ps.size) AdtConsExpr(c, ps)
//          else throw new ParsingException(s"Constructor $c expected $nparams parameters, but ${ps.size} found")
//        case Some(CONNNAME) =>
//            ConnId(c, ps)
//        //TODO: @Guille - some symbols were not captured. Check what you had in mind.
//        case Some(x) => throw new ParsingException(s"Unexpected identifier $c with symbol $x")
//        //TODO: @Guille - None was not captured. Check what you had in mind.
//        case None => throw new ParsingException(s"No symbol found for identifer $c.")
//
//      }
//    }
//
//  /**
//    * Expressions that can appear in actual parameters
//    * dataExpr |
//    * dataExpr, dataExpr+
//    * @return a list containing an expression for each parameter found
//    */
//  def paramExprs:Parser[List[Expr]] =
//    dataExpr ~ rep("," ~> dataExpr) ^^ { case e1~e2 => e1::e2 }
//
//  /* Auxiliary functions */
//
//  /**
//    * Number of parameters for a given ADT constructor
//    * @param adtConst the name of an adt constructor
//    * @return
//    */
//  private def sizeOfParams(adtConst:String):Int = {
//    var variant =  adts.flatMap(t => t.constructors).find(v => v.name == adtConst)
//    if (variant.isDefined)
//      variant.get.param.size
////      variant.get match {
////       case AdtVal(n) => throw new ParsingException("An ADT Value Variant has no Parameters: ")
////       case AdtConst(n,p) => p.size
////      }
//    else
//      throw new ParsingException("Unknown Constructor: " + adtConst)
//  }
}
