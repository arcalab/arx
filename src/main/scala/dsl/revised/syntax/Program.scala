package dsl.revised.syntax

import dsl.revised.core.Network.Constructor
import dsl.revised.core.Term.IntVal
import dsl.revised.core.{Automaton, Term}
import dsl.revised.syntax.Program.Module
import dsl.revised.typing.Type

final case class Program(main:Module, modules:Map[String,Module]):
  override def toString: String = Show(this)

object Program:
  final case class Module(imports: List[String],
                          declarations: List[Decl])

  /** A declaration is any line of the module that is not an import one:
    * data, automata, network, link, or return. */
  sealed abstract class Decl
  case class DataDecl(name:String, args:List[String], const: List[Constructor]) extends Decl
  case class AutDecl(name:String, args:List[String], inputs:List[String], outputs:List[String], aut:Automaton) extends Decl
  case class NetDecl(name:String, args:List[String], inputs:List[String], outputs:List[String], decls:List[Decl]) extends Decl
  case class LinkDecl(invoc:InputCall, outputs:List[String]) extends Decl

  /** An inputcall is used on the LHS of the link declarations.
    * It can be a port name or a call to a connector (automata or network). */
  sealed abstract class InputCall
  case class PortCall(name:String) extends InputCall
  case class ConnCall(name:String, args:List[Term], inputs:List[InputCall]) extends InputCall


  ///////////////////
  // Experimenting //
  ///////////////////

  object Examples:
    import Automaton.Examples._

    def enumerate(s:String,constr:String*): DataDecl =
      DataDecl(s,Nil,constr.toList.map(c=>Constructor(c,Nil)))
    val a = "a"; val b = "b"
    val m = "m"; val n = "n"

    val myMain = Module(Nil,List(
      DataDecl("Fruit",Nil,List(Constructor("Apple",Nil),Constructor("Pear",Nil))),
      AutDecl("fifo",Nil,List("a"),List("b"),fifo("a","b","r")),
      AutDecl("fifof",List("m"),List("a"),List("b"),fifofull("a","b","r","m")),
      AutDecl("timer",List("n"),List("a"),List("b"),timer("a","b","r","t","n")),
      NetDecl("fifo2",Nil,List("a"),List("b"),List(
        LinkDecl(ConnCall("fifo",Nil,List(ConnCall("fifof",List(Term.Fun("Apple",Nil)),List(PortCall("a"))))),List("b"))
      )),
      LinkDecl(ConnCall("fifo2",Nil,List(PortCall("x"))),List("y")),
      LinkDecl(ConnCall("timer",List(IntVal(5)),List(PortCall("y"))),List("z"))
    ))
    val myProgram = Program(myMain,Map())
