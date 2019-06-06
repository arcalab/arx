import common.ParsingException
import dsl.{AST, Parser, TypeInference}
//import dsl.TypeInference._

/**
  * Created by guillecledou on 2019-06-04
  */


object DSL {


  def parse(code:String):AST = Parser.parse(code) match {
    case Parser.Success(result, next) => result
    case f:Parser.NoSuccess => throw new ParsingException("Parser failed: "+f.msg)
  }

  def infer(ast: AST) = TypeInference.infer(ast)

}
