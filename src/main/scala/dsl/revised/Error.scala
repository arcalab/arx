package dsl.revised

object Error:
  class TypeError(msg:String) extends RuntimeException(msg)
  class ParsingError(msg:String) extends RuntimeException(msg)
  class InvalidProgramError(msg:String) extends RuntimeException(msg)

  def typing(msg:String) = throw new TypeError(msg)
  def parsing(msg:String) = throw new ParsingError(msg)
  def invalid(msg:String) = throw new InvalidProgramError(msg)

  // Use this to show/hide debug information
  def debug(msg:String)(using who:String=""): Unit =
    //if who=="" then println(msg) else println(s"[$who] $msg")
    {}

