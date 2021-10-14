package dsl.revised.syntax

import cats.parse.Parser
import cats.parse.LocationMap
import cats.parse.Numbers._
import cats.syntax.all._
import Parser._
import cats.data.NonEmptyList
import cats.parse.Rfc5234.{alpha, sp}


object ParserTest:

  sealed abstract class Binary extends Product with Serializable
  object Binary:
    case object Zero extends Binary
    case object One extends Binary
  val one: Parser[Binary] =
    char('0').as(Binary.Zero) orElse char('1').as(Binary.One)

  //"B2"
  final case class LetterAndNumber(value: String) extends AnyVal
  val two: Parser[LetterAndNumber] =
    (alpha ~ digit).string.map(LetterAndNumber.apply)

  //"1011"
  final case class BinaryList(value: NonEmptyList[Binary]) extends AnyVal
  val three: Parser[BinaryList] =
    val binary = char('0').as(Binary.Zero) orElse char('1').as(Binary.One)
    binary.rep.map(BinaryList.apply)

  //"Sam O'Brian"
  final case class Name(value: String) extends AnyVal
  val four: Parser[Name] =
    val allowedChars = List(char('\''), alpha, sp)
    (alpha ~ oneOf(allowedChars).rep).string.map(Name.apply)

  final case class Score(left: Int, right: Int)
  val five: Parser[Score] =
    val multiDigit = digit.rep.string.map(_.toInt)
    ((multiDigit <* char('-').surroundedBy(sp)) ~ multiDigit).map(Score.apply.tupled)

  //"one,2,three"
  sealed abstract class MyTuple extends Product with Serializable
  object MyTuple:
    final case class Two(one: String, two: String) extends MyTuple
    final case class Three(one: String, two: String, three: String) extends MyTuple
  val six: Parser[MyTuple] =
    val comma = char(',')
    val item = alpha.rep.string
    val two = ((item <* comma) ~ item).map(MyTuple.Two.apply.tupled)
    val three = (item ~ item.surroundedBy(comma) ~ item).map {
      case ((a, b), c) => MyTuple.Three(a, b, c)
    }
    three.backtrack orElse two

  final case class UserName(value: String) extends AnyVal
  val seven: Parser[UserName] =
    val name = alpha.rep.string
    ((name.soft ~ char('.')).rep0.with1 ~ name).string.map(UserName.apply)
