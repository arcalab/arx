package dsl.revised.syntax

import dsl.revised.Prelude
import dsl.revised.core.Network.Constructor
import dsl.revised.core.Term.IntVal
import dsl.revised.core.{Automaton, Term}
import dsl.revised.syntax.Program.Module
import dsl.revised.typing.Type

final case class Program(main:Module, modules:Map[String,Module]):
  override def toString: String = Show(this)

object Program:
  final case class Module(imports: List[String],
                          declarations: List[Decl]):
    def +(imp:String): Module = Module(imp::imports,declarations)

  /** A declaration is any line of the module that is not an import one:
    * data, automata, network, link, or return. */
  enum Decl:
    case DataDecl(name:String, args:List[String], const: List[Constructor])
    case AutDecl(name:String, args:List[String], inputs:List[String], outputs:List[String], aut:Automaton)
    case NetDecl(name:String, args:List[String], inputs:List[String], outputs:List[String], decls:List[Decl])
    case LinkDecl(invoc:InputCall, outputs:List[String])
    case ConstDecl(name:String, term:Term)

  /** An inputcall is used on the LHS of the link declarations.
    * It can be a port name or a call to a connector (automata or network). */
  enum InputCall:
    def name:String
    case PortCall(name:String)
    case ConnCall(name:String, args:List[Term], inputs:List[InputCall])


  
  ///////////////////
  // Experimenting //
  ///////////////////

  object Examples:
    import Automaton.Examples._
    import Program.Decl._
    import Program.InputCall._

    def enumerate(s:String,constr:String*): DataDecl =
      DataDecl(s,Nil,constr.toList.map(c=>Constructor(c,Nil)))
    val a = "a"; val b = "b"
    val m = "m"; val n = "n"

                        // module "reo" defined in Prelude and included in Encode
    val myMain = Module(List("reo"),List(
      DataDecl("Fruit",Nil,List(Constructor("Apple",Nil),Constructor("Pear",Nil))),
//      AutDecl("fifo",Nil,List("a"),List("b"),fifo("a","b","r")),
//      AutDecl("fifof",List("m"),List("a"),List("b"),fifofull("a","b","r","m")),
//      AutDecl("timer",List("n"),List("a"),List("b"),timer("a","b","r","t","n")),
      NetDecl("fifo2",Nil,List("a"),List("b"),List(
        LinkDecl(ConnCall("var",Nil,List(ConnCall("fifofull",List(Term.Fun("Apple",Nil)),List(PortCall("a"))))),List("b"))
      )),
      LinkDecl(ConnCall("fifo2",Nil,List(PortCall("x"))),List("y")),
      LinkDecl(ConnCall("timer",List(IntVal(5)),List(PortCall("y"))),List("z"))
    ))
    val myProgram = Program(myMain,Map())
