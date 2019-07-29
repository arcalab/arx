package dsl.analysis.syntax

import SymType._
import dsl.common.ParsingException

/**
  * Created by guillecledou on 2019-06-17
  */


class SymbolsTable {

  // for now only one scope
  protected val hash: Map[String, SymType] = Map()

  /**
    * Get the type of a symbol if it is declared
    * @param sym
    * @return
    */
  def apply(sym:String):Option[SymType] =
    if (hash.contains(sym)) Some(hash(sym)) else None

  /**
    * Check if a symbol exists
    * @param sym
    * @return
    */
  def contains(sym:String):Boolean = hash.contains(sym)

  /**
    * Add a new symbol and the type of symbol to the table
    * If there exists some conflict throws an ParsingException
    * @param sym
    * @param symType
    * @return
    */
  def add(sym: String, symType: SymType): SymbolsTable =  {
    if (this.conflict(sym,symType)) {
      println("Symbols table:"+ hash)
      println(s"trying to add symbol $sym as $symType")
      throw new ParsingException(s"Symbol name $sym is already used within the scope")
    }
    else {
      val old = hash
      new SymbolsTable {
        override val hash = old + (sym -> symType)
      }
    }
  }

  /**
    * Checks if there is a conflict when adding a new symbol:
    *   - there is a conflict with any name sym, if it exists as a TYPE,CONST or FUN
    *     - in addition, there is a conflict with a fun name, if it exists as anything
    *   - there is a conflict with a data or input param, if it exists as anything
    *   - there is never a conflict if it doesn't exists
    * @param sym
    * @return whether there is a conflict
    */
  private def conflict(sym:String,symType:SymType):Boolean = this(sym) match {
    case None => false
    case Some(t) => ((symType == FUN) && this.contains(sym)) ||
      Set(TYPE,CONST,FUN).contains(t) ||
      (Set(INPUT,DATA).contains(symType) && this.contains(sym))
  }
}
