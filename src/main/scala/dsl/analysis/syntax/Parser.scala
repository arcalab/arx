package dsl.analysis.syntax

import dsl.analysis.syntax.Program.Block
import dsl.analysis.syntax.SymbolType._
import dsl.analysis.types.{Pull, Push}
import dsl.backend.{Import, Prelude, Show}
import dsl.common.ParsingException

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

/**
  * Created by guillecledou on 2019-05-31
  */

object Parser extends RegexParsers {

  private var sym: SymbolsTable = new SymbolsTable
  //todo: fix error with fun def not creating new level in symbols table
  def parseProgram(code:String):ParseResult[Program] = {
    sym = new SymbolsTable
    parseAll(program,code)
  }

  def parseFunction(code:String):ParseResult[Statement] = {
    sym = new SymbolsTable
    parseAll(funDef,code)
  }

  override def skipWhitespace = true
  override val whiteSpace: Regex = "( |\t|\r|\f|\n|//.*)+".r

  val capitalId:Parser[String] = """[A-Z][a-zA-Z0-9_]*""".r
  val lowerCaseId:Parser[String] = """[a-z][a-zA-Z0-9_]*""".r //possible can just be represented as a regular typename
  val id:Parser[String] = """[a-zA-Z][a-zA-Z0-9_]*""".r

  // todo: use it
  val keywords: Set[String] =
    Set("data","def","match","build") ++ Prelude.primitiveFunctionNames()

  /* Program */

  def program:Parser[Program] =
    rep(imports) ~
    rep(dt) ~ block ^^ {
      case imp~typs~bl => Program(imp,typs,bl)
    }

  def imports:Parser[Import] =
    "import"~capitalId~rep("."~>capitalId)~opt(members) ^^ {
      case _~m~ms~mem => Import((m::ms).mkString("."),mem.getOrElse(List()))
    }

  def members:Parser[List[String]] =
    ".{"~>repsep(id,",")<~"}" |
    "."~>id ^^ {List(_)}

  def block: Parser[Block] =
    rep(statement)

  def statement: Parser[Statement] = {
    funDef | assignment | strExpr
  }

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


  def dt:Parser[TypeDecl] =
    "data" ~ capitalId ~ opt(tparams) ~ "=" ~ constructors ^^ {
      case _~n~tps~_~vs => sym=sym.add(n,TYPE);TypeDecl(ConTypeName(n,tps.getOrElse(List())),vs)}

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

  def funDef: Parser[Statement] =
    "def" ~> log(restFunDef)("restFunDef")


  def restFunDef:Parser[Statement] = {
    sym = sym.addLevel();
    //println("entering fun def")
    lowerCaseId /* ~ opt("<" ~> dataFormalParams <~ ">")*/ ~ funParamsAndBody ^? ({
      case f ~ rest  if !keywords.contains(f) =>
        sym = sym.add(f, FUN) // quick hack to see if function name is not used/delared inside the function todo: fix it
        sym = sym.rmLevel()
        sym = sym.add(f, FUN) // actually added it to the current scope
        rest match {
          case Right((typ,sf)) => SFunDef(f, typ, sf)
          case Left((ps,typ, bl)) => FunDef(f,ps, typ, bl)}},
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
    lowerCaseId~opt(":"~>tname)~opt("[!|?]".r) ^^ {
      case v~t~None => TypedVar(v,t,None)
      case v~t~Some(r) => TypedVar(v,t,if (r.matches("!")) Some(Push) else Some(Pull))
    }

  /* Assignments */

  def assignment:Parser[Statement] =
    repsep(lowerCaseId,",")~"<-|<~".r~strExpr ^^ {
      case ids~typ~expr =>
        ids.foreach(id => sym = sym.add(id,VAR))
        if (typ.matches("<-")) Assignment(ids,expr)
        else RAssignment(ids,expr)
    }

}
