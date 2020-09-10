import dsl._
import dsl.analysis.semantics.Encode
import dsl.analysis.syntax._
import dsl.backend.{ArxNet, Show}
import dsl.common.ParsingException
import org.scalatest.FlatSpec

/**
  * Created by guillecledou on 2019-06-03
  */


class TestEncode extends FlatSpec {


//  OK(s"""def alt(a) = {
//        |  lossy(x) x<-fifo(a)
//        |}
//        |alt(q)
//        |alt(w)
//     """.stripMargin)

//  OK(s"""drain(a,b)
//        |x<-a
//        |x<-fifo(b)
//        |x
//     """.stripMargin)

  OK(s"""out1<-in out2<-in
        |out1 out2
        |
     """.stripMargin)

//  OK(
//    s"""
//       |fifo(x)
//     """.stripMargin)

  def OK(code:String) =
    s"The program $code" should "be encoded" in {
//      assert(Parser.parse(code).isInstanceOf[Parser.Success[AST]])
      val prog = DSL.parse(code)
      val (tprog,tctx) = DSL.typeCheck(prog)
      //val (sbprog,sbOuts,sbCtx) = DSL.encode(tprog,tctx)
//      val sbCtx = DSL.encode(tprog,tctx)

      val net = new ArxNet
      val (sb,sbOuts,sbCtx)  =  Encode(tprog,tctx,net)
      println("[TEST] Encoded net: "+net)
      println("[TEST] Encoded sb: "+Show(sb))
      sbCtx.add("Program",(sb,List(),sbOuts,net))
    }


  def notOK(code:String) =
    s"The program $code" should "throw a ParsingException" in {
    assertThrows[ParsingException] {Parser.parseProgram(code)}
  }

}
