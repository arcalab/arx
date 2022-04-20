package dsl.revised

object Error:
  class TypeError(msg:String) extends RuntimeException(msg)
  class ParsingError(msg:String) extends RuntimeException(msg)
  class EncodingError(msg:String) extends RuntimeException(msg)

  def typing(msg:String) = throw new TypeError(msg)
  def parsing(msg:String) = throw new ParsingError(msg)
  def encoding(msg:String) = throw new EncodingError(msg)

  case class Who(name:String)
  // Use this to show/hide debug information
  def debug(msg:String)(using who:Who=Who("")): Unit =
//    if who.name=="" then println(msg) else println(s"[$who] $msg")
    {}

