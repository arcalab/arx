package dsl.backend

import dsl.analysis.syntax
import dsl.analysis.syntax.{Assignment2, Const, FunDef2, FunctionApp, GroundTerm, Program, StreamExpr}
import dsl.analysis.syntax.Program.Block
import dsl.backend.Net.{Connector, Port}


/**
  * Network of primitives - to be translated as a preo.Network, that can in turn be
  * simplified, translated into a preo.Circuit, and depicted using a JS library.
  * @param prims
  * @param ins
  * @param outs
  */
case class Net(prims:List[Connector], ins:List[Port], outs:List[Port])

object Net {

  case class Connector(name: String, in: Int, out: Int)

  type Port = Int

  case class Context(fun: Map[String, (Int, Int)], ports: Map[Port, PType]) {
    def +(other: Context): Context =
      Context(fun ++ other.fun, ports ++ other.ports)
  }

  sealed abstract class PType

  case object In extends PType

  case object Out extends PType

  case object Mix extends PType

  private var seed = 0

  def apply(prog: Program): Net = {
    seed = 0
    apply(prog.block, Context(Map(), Map()))
  }

  def apply(b: Block, gamma: Context): Net = b match {
    case Nil => Net(Nil, Nil, Nil)
    // TODO: changing here. Goal: generate a Net (and later convert it to a preo.Network to be depicted
    case st :: tail => st match {
      case expr: StreamExpr => expr match {
        case term: GroundTerm => term match {
          case syntax.Port(x) => apply(Nil, gamma) //primConn(x)
          case Const(q, args) => apply(Nil, gamma) //wrtConn(Show(term))
        }
        case FunctionApp(sfun, args) => apply(Nil, gamma)
      }
      case FunDef2(name, params, typ, block) => apply(Nil, gamma)
      case Assignment2(variables, expr) => apply(Nil, gamma)
    }
  }
}