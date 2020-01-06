package dsl.analysis.syntax

/**
  * Created by guillecledou on 2019-06-17
  */


object SymbolType extends Enumeration {
  type SymbolType = Value
  val TYPE  /* a name of a data type */
    , CONST /* a constructor name */
    , VAR   /* a variable (not a input parameter) */
    , FUN   /* a function name */
    , DATA  /* data formal params e.g. to initialize functions that receive data parameters (dropped for now) */
    , INPUT /* a formal parameter (probably not needed) */
    = Value

    // val TYPENAME, ADTVAL, ADTCONST,VARNAME,CONNNAME = Value
}