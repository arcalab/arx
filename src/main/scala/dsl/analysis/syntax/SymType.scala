package dsl.analysis.syntax

/**
  * Created by guillecledou on 2019-06-17
  */

object SymType extends Enumeration {
  type SymType = Value
  val TYPE,CONST,VAR,FUN,DATA,INPUT = Value
  //  val TYPENAME, ADTVAL, ADTCONST,VARNAME,CONNNAME = Value
}

