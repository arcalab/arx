import dsl.DSL
//import dsl.analysis.semantics.{BaseType, TypeExpr}
import dsl.analysis.syntax._
import dsl.common.{ParsingException, UndefinedVarException}
import org.scalatest.flatspec.AnyFlatSpec

/**
  * Created by guillecledou on 2019-06-07
  */


class TestTypeInference extends AnyFlatSpec{

  /*
  val bool = BaseType("Bool",List())
  val nat = BaseType("Nat",List())
  val boolList = BaseType("List",List(bool))
  val natList = BaseType("List",List(nat))

  val map:Map[String,TypeExpr] = Map(
    "x" -> boolList,
    "y" -> boolList,
    "z" -> boolList,
    "w" -> boolList,
    "s" -> natList,
    "o" -> TProd(boolList,TOpt(boolList)),
    "o1" -> boolList,
    "o2" -> TOpt(boolList),
    "o3" -> nat,
    "o4" -> TOpt(nat),
    "out1" -> nat,
    "out2" -> TOpt(nat)
  )

  OK(s"""
        |data List<a> = Nil | Cons(a,List<a>)
        |data Bool = True | False
        |data Nat = Zero | Succ(Nat)
        |
        |x <- Nil
        |y <- Cons(True,Nil)
        |z <- Cons(True,Cons(False,x))
        |w <- Cons(False,z)
        |s <- Cons(Zero,Nil)
     """.stripMargin)

  OK(s"""
       |data List<a> = Nil | Cons(a,List<a>)
       |data Bool = True | False
       |data Nat = Zero | Succ(Nat)
       |
       |def conn(x) = {
       |	//dupl;fifo*lossy
       |  out<-fifo(x) out<-lossy(x)
       |  out
       |}
       |
       |y <- Cons(True,Nil)
       |o <- conn(y)
       |o1,o2 <- conn(y)
       |o3,o4 <- conn(Zero,out1,out2)
     """.stripMargin)

  illegalParam(
    s"""
       |data Nat = Zero | Succ(Nat)
       |
       |def conn = {
       |	//dupl;fifo*lossy
       |  out<-fifo(x) out<-lossy(x)
       |  out
       |}
       |
       |o <- conn(Zero,True)
     """.stripMargin)

  illegalParam(
    s"""
       |data List<a> = Nil | Cons(a,List<a>)
       |data Bool = True | False
       |data Nat = Zero | Succ(Nat)
       |
       |	//dupl;fifo*lossy
       |  out<-fifo(x) out<-lossy(x)
       |  out
       |}
       |o <- conn(Zero,o1,o2,o3)
     """.stripMargin)

  undefinedId(s"""
        |data List<a> = Nil | Cons(a,List<a>)
        |data Bool = True | False
        |
        |x <- Nild
        |y <- Cons(True,x)
     """.stripMargin)

  incompatibleType(
    s"""
       |data Bool = True | False
       |data Nat = Zero | Succ(Nat)
       |
       |x <- Zero
       |y <- Cons(True,Cons(x,Nil))
     """.stripMargin)

  def OK(code:String) =
    s"The variables in the program $code" should "be correctly typed" in {
      var ast = DSL.parse(code)
      var types = DSL.typeCheck(ast)
      types.map(v => assertResult(map(v._1))(v._2))
    }


  def incompatibleType(code:String) =
    s"The program $code" should "throw a TypeException" in {
      assertThrows[ParsingException] {
        DSL.typeCheck(DSL.parse(code))
      }
    }

  def undefinedId(code:String) =
    s"The program $code" should "throw a UndefinedVarException" in {
      assertThrows[UndefinedVarException] {
        DSL.typeCheck(DSL.parse(code))
      }
  }

  def illegalParam(code:String) =
    s"The program $code" should "throw an IllegalParameterException" in {
      assertThrows[IllegalParameterException] {
        DSL.typeCheck(DSL.parse(code))
      }
    }

   */
}
