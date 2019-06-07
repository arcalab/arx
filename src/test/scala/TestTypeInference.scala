import dsl.DSL
import dsl.analysis.semantics.{BaseType, TypeExpr}
import dsl.analysis.syntax._
import dsl.common.{ParsingException, UndefinedVarException}
import org.scalatest.FlatSpec

/**
  * Created by guillecledou on 2019-06-07
  */


class TestTypeInference extends FlatSpec{

  val bool = BaseType("Bool",List())
  val nat = BaseType("Nat",List())
  val boolList = BaseType("List",List(bool))
  val natList = BaseType("List",List(nat))

  val map:Map[String,TypeExpr] = Map(
    "x" -> boolList,
    "y" -> boolList,
    "z" -> boolList,
    "w" -> boolList,
    "s" -> natList
  )

  OK(s"""
        |data List<a> = Nil | Cons(a,List<a>)
        |data Bool = True | False
        |data Nat = Zero | Succ(Nat)
        |
        |x = Nil
        |y = Cons(True,Nil)
        |z = Cons(True,Cons(False,x))
        |w = Cons(False,z)
        |s = Cons(Zero,Nil)
     """.stripMargin)

  undefinedId(s"""
        |data List<a> = Nil | Cons(a,List<a>)
        |data Bool = True | False
        |
        |x = Nild
        |y = Cons(True,x)
     """.stripMargin)

  incompatibleType(
    s"""
       |data Bool = True | False
       |data Nat = Zero | Succ(Nat)
       |
       |x = Zero
       |y = Cons(True,Cons(x,Nil))
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
}
