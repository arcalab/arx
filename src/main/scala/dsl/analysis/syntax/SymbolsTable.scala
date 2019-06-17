package dsl.analysis.syntax

import SymbolType.SymbolType
import dsl.common.{ParsingException, UndefinedVarException}

/**
  * Created by guillecledou on 2019-06-17
  */


class SymbolsTable {

  // for now only one scope
  protected val hash: Map[String, SymbolType] = Map()


  /**
    * Add a new symbol and the type of symbol to the table
    * If it exists throws an MultipleDeclarationException
    * @param sym
    * @param symType
    * @return
    */
  def add(sym: String, symType: SymbolType): SymbolsTable = {
    // if the symbol exists and is not a variable then it is multiple declaration error
    if (hash.contains(sym) && hash(sym)!=SymbolType.VARNAME)
      throw new ParsingException(s"Symbol name $sym is already declared within the scope")
    else {
      val old = hash
      new SymbolsTable {
        override val hash = old + (sym -> symType)
      }
    }
  }

  /**
    * Check if a symbol exists
    * @param sym
    * @return
    */
  def contains(sym:String):Boolean = hash.contains(sym)


  /**
    * Get the type of a symbol if it is declared, otherwise throws an UndefinedException
    * @param sym
    * @return
    */
  def apply(sym:String):SymbolType =
    if (hash.contains(sym)) hash(sym)
    else throw new UndefinedVarException(s"Unknown symbol: ${sym}")
}


object SymbolType extends Enumeration {
  type SymbolType = Value
  val TYPENAME, ADTVAL, ADTCONST,VARNAME,CONNNAME = Value
}

