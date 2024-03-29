import dsl.analysis.syntax._
import org.scalatest.flatspec.AnyFlatSpec
import dsl._
import common.ParsingException
import dsl.DSL.toVar

import scala.util.Success

/**
  * Created by guillecledou on 2019-06-03
  */


class TestParser extends AnyFlatSpec {

  /*
  var t1 = TypeDecl(
    // type name with params
    ConTypeName("List",List(AbsTypeName("a"))),
    // variants of the type
    List( AdtVal("Nil"),
          AdtConst("Cons",List(AbsTypeName("a"),ConTypeName("List",List(AbsTypeName("a")))))))

  var t2 = TypeDecl(
    // type name with no params
    ConTypeName("Bool",List()),
    // variants of the type
    List( AdtVal("True"),AdtVal("False")))

  var asg1 =  "x" := Nil//Assignment(List("x"),AdtTerm("Nil"))
  var asg2 = "y" := //Assignment(Identifier("y"),AdtConsExpr("Cons",List(AdtTerm("True"),AdtTerm("Nil"))))
  var asg3 = Assignment(Identifier("z"),AdtConsExpr("Cons",List(AdtTerm("True"),
    AdtConsExpr("Cons",List(AdtTerm("False"),AdtTerm("Nil"))))))
  var asg4 = Assignment(Identifier("x"),Identifier("Nild"))
  var asg5 = Assignment(Identifier("y"),AdtConsExpr("Cons",List(AdtTerm("True"),Identifier("x"))))

  var p1 = Statements(List(t1,t2,asg4,asg2,asg3))
  var p2 = Statements(List(t1,t2,asg1,asg5))


  OK(s"""
       |data List<a> = Nil | Cons(a,List<a>)
       |data Bool = True | False
       |
       |x <- Nild
       |y <- Cons(True,Nil)
       |z <- Cons(True,Cons(False,Nil))
     """.stripMargin,p1)

  OK(s"""
        |data List<a> = Nil | Cons(a,List<a>)
        |data Bool = True | False
        |
        |x <- Nil
        |y <- Cons(True,x)
     """.stripMargin,p2)

  notOK(
    s"""
       |data Bool = True | False
       |
       |y <- Cons(True,Nil)
     """.stripMargin)

  def OK(code:String,ast:AST) =
    s"The program $code" should "be accepted by the parser" in {
//      assert(Parser.parse(code).isInstanceOf[Parser.Success[AST]])
      Parser.parseProgram(code) match {
        case Parser.Success(res, next) => assertResult(ast)(res)
        case f:Parser.NoSuccess => throw new ParsingException("Parser failed: "+f.msg)
      }
    }


  def notOK(code:String) =
    s"The program $code" should "throw a ParsingException" in {
    assertThrows[ParsingException] {Parser.parseProgram(code)}
  }
  */
}
