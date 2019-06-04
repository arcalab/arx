import java.security.cert.CertificateParsingException

import org.scalatest.FlatSpec
import dsl._
import common.ParsingException

import scala.util.Success

/**
  * Created by guillecledou on 2019-06-03
  */


class TestParser extends FlatSpec {

  OK(s"""
       |data List<a> = Nil | Cons(a,List<a>)
       |data Bool = True | False
       |
       |x = Nild
       |y = Cons(True,Nil)
       |z = Cons(True,Cons(False,Nil))
     """.stripMargin)

  OK(s"""
        |data List<a> = Nil | Cons(a,List<a>)
        |data Bool = True | False
        |
        |x = Nil
        |y = Cons(True,x)
     """.stripMargin)

  notOK(
    s"""
       |data Bool = True | False
       |
       |y = Cons(True,Nil)
     """.stripMargin)

  def OK(code:String) =
    s"The program $code" should "be accepted by the parser" in {
      assert(Parser.parse(code).isInstanceOf[Parser.Success[AST]])
    }


  def notOK(code:String) =
    s"The program $code" should "throw a ParsingException" in {
    assertThrows[ParsingException] {Parser.parse(code)}

  }

}
