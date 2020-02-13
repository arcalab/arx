package dsl.analysis.syntax

import SymbolType._
import dsl.common.{ParsingException, UndefinedVarException}

/**
  * Created by guillecledou on 2019-06-17
  */


class SymbolsTable {

//  protected val hash: Map[String, SymbolType] = Map()
  protected val tables:List[Map[String,SymbolType]] = List(Map())


  /**
    * Get the type of a symbol if it is declared
    * @param sym
    * @return
    */
  def apply(sym:String):Option[SymbolType] =
    if (tables.last.contains(sym)) Some(tables.last(sym)) else None

  /**
    * Check if a symbol exists
    * @param sym
    * @return
    */
  def contains(sym:String):Boolean = tables.last.contains(sym)

  /**
    * Add a new symbol and the type of symbol to the table
    * If there exists some conflict throws an ParsingException
    * @param sym
    * @param symType
    * @return
    */
  def add(sym: String, symType: SymbolType): SymbolsTable =
    if (this.conflict(sym,symType)) {
      throw new ParsingException(s"Symbol name $sym is already used within the scope")
    }
    else
      if ((symType == VAR) && this.contains(sym)) this
      else {
        val old = tables
        new SymbolsTable {
          override val tables = old.init++List(old.last + (sym -> symType))
        }
      }

  def addLevel():SymbolsTable = {
    val old = tables
    new SymbolsTable {
      override val tables = old++List(old.last)
    }
  }

  def rmLevel():SymbolsTable = {
    val old = tables.init
    new SymbolsTable {
      override val tables = old
    }
  }


  /**
    * Checks if there is a conflict when adding a new symbol:
    *   - there is a conflict with any name sym, if it exists as a TYPE,CONST or FUN
    *     - in addition, there is a conflict with a fun name, if it exists as anything
    *   - there is a conflict with a data or input param, if it exists as anything // shouldn't be a problem, right?
    *   - there is never a conflict if it doesn't exists
    * @param sym
    * @return whether there is a conflict
    */
  private def conflict(sym:String,symType:SymbolType):Boolean = {
    //println(s"Checking conflict for ${sym} in tables: ${tables} ")
    val res = this(sym) match {
      case None => false
      case Some(t) => ((symType == FUN) && this.contains(sym)) ||
        Set(TYPE,CONST,FUN).contains(t) //||
      //(Set(INPUT,DATA).contains(symType) && this.contains(sym))
    }
    res
  }
}

