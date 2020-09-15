//package dsl.analysis.types
//
//import dsl.backend.PortType
//
///**
//  * Created by guillecledou on 2019-08-01
//  */
//
//sealed trait ContextEntry {
//  /* type expression associated to the entry */
//  val tExp:TExp
//}
//
//case class FunEntry(/*tParams:List[TExp] ,*/ tExp:TFun, funCtx:Context, dps:List[TExp]=List()) extends ContextEntry
//case class TypeEntry(tExp:TBase, constructors:List[ConstEntry])                         extends ContextEntry
//case class ConstEntry(name:String, params:List[TExp], tExp:TExp)                        extends ContextEntry
//case class PortEntry(tExp:TExp, pType:PortType)                                         extends ContextEntry
//case class VarEntry(tExp:TExp)  extends ContextEntry