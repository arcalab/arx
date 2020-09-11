//package dsl.analysis.syntax
// DEPRECATED
//import dsl.analysis.semantics._
//import dsl.backend.Show
//
//import scala.util.matching.Regex
//import scala.util.parsing.combinator.RegexParsers
//
///**
//  * Created by guillerminacledou on 30/07/2020
//  */
//
//
//object SBParser extends RegexParsers {
//
//  def parseSB(sbs:String):ParseResult[StreamBuilder] = {
//    parseAll(sb,sbs)
//  }
//
//
//  override def skipWhitespace = true
//  override val whiteSpace: Regex = "( |\t|\r|\f|\n|//.*)+".r
//
//  val lowerCaseId:Parser[String] = """[a-z][a-zA-Z0-9_]*""".r
//  val upperCaseId:Parser[String] = """[A-Z][a-zA-Z0-9_]*""".r
//  val num:Parser[Int] = """[0-9][0-9]*""".r ^^ {case n => n.toInt}
//
//  def sb:Parser[StreamBuilder] =
//    rep(sbdef) ~
//      sbcomp ^^ {
//      case defs~comps =>
//        // todo: move this out of the parser later
//        val definitions:Map[String,StreamBuilder] = defs.flatMap(_.toMap).toMap
//        if (!comps.forall(definitions.contains))
//          throw new RuntimeException(s"Undefined ids in composition")
//        val comp = comps.map(sbid => definitions(sbid))
//        val sbres = comp.foldRight[StreamBuilder](StreamBuilder.empty)(_*_)
//        Show(sbres)
//        sbres
//    }
//
//  def sbcomp:Parser[List[String]] =
//    rep1sep(lowerCaseId,"*")
//
//
//  def sbdef:Parser[Map[String,StreamBuilder]] =
////    "def"~
//      lowerCaseId~opt("<"~>rep1sep(lowerCaseId,",")<~">")~"="~"{"~opt(initS)~rep1sep(gc,";")~"}" ^^ {
//      case id~ms~_~_~initS~gcs~_ =>
//        val mems = ms.getOrElse(List()).toSet
//        val ins= gcs.flatMap(gc => gc.inputs).toSet -- mems
//        val outs = gcs.flatMap(gc => gc.outputs).toSet -- mems
//        Map(id->StreamBuilder(initS.getOrElse(Set()),gcs.toSet,ins,outs,mems))
//    }
//
//  def initS:Parser[Set[Command]] =
//    "init" ~> rep1sep(cmd, ",") ^^ { case cs => cs.toSet}
//
//
//  def gc:Parser[GuardedCommand] =
//    guard~"->"~repsep(cmd,",") ^^ {case g~_~cs => (g -> cs.toSet)}
//
//  def guard:Parser[Guard] =
//    repsep(guardItem,",") ^^ {
//      case gs => Guard(gs.foldRight[Set[GuardItem]](Set())(_.union(_)))}
//
//  def guardItem:Parser[Set[GuardItem]] =
//    gets | asks | isQs | unds
//  //{
////    "get" ~> guardParam ^^ (ps => ps.map(s => Get(s))) |
////      "und" ~> guardParam ^^ (ps => ps.map(s => Und(s))) |
////      "ask" ~> guardParam ^^ (ps => ps.map(s => Ask(s))) |
////      "is" ~ upperCaseId ~ "(" ~ term ~ ")" ^^ { case _ ~ v ~ _ ~ t ~ _ => Set(IsQ(v, t)) }
////  }
//
//  def gets:Parser[Set[GuardItem]] =
//    "get" ~> guardParam ^^ (ps => ps.map(s => Get(s)))
//
//  def asks:Parser[Set[GuardItem]] =
//    "ask" ~> guardParam ^^ (ps => ps.map(s => Ask(s)))
//
//  def unds:Parser[Set[GuardItem]] =
//    "und" ~> guardParam ^^ (ps => ps.map(s => Und(s)))
//
//  def isQs:Parser[Set[GuardItem]] =
//    "is" ~ upperCaseId ~ "(" ~ term ~ ")" ^^ { case _ ~ v ~ _ ~ t ~ _ => Set(IsQ(v, t)) }
//
//  def guardParam:Parser[Set[String]] =
//    "("~>rep1sep(lowerCaseId,",")<~")" ^^ (ports => ports.toSet)
//
//  def cmd:Parser[Command] =
//    lowerCaseId~":="~term ^^ { case v~_~t =>Command(v,t)}
//
//  def term:Parser[Term] =
//    lowerCaseId ^^ {Var} |
//    "Get"~"<"~upperCaseId~","~num~">"~"("~term~")" ^^ {
//      case _~_~tp~_~i~_~_~t~_ => GetQ(tp,i,t)} |
//    upperCaseId~opt("("~>rep1(term)<~")") ^^ {case v~ts => Q(v,ts.getOrElse(List()))} |
//    "_" ^^ {case _ => Q("_",List())}
//
//  def fun(b:Boolean): Set[GuardItem] =
//    if (b) Set(Get("b")) else Set(Ask("b"))
//}
