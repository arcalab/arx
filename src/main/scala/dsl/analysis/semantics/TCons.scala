//package dsl.analysis.semantics
//
///**
//  * Created by guillecledou on 2019-06-07
//  */
//
///* T1 = T2 */
//case class TCons(l:TypeExpr, r:TypeExpr) {}
//
//sealed trait TypeExpr {
//  def substitute(tvar:TVar,te:TypeExpr):TypeExpr
//}
//
///* ADT type */
//case class BaseType(name:String,param:List[TypeExpr]) extends TypeExpr {
//  def substitute(tvar:TVar,te:TypeExpr):BaseType = {
//    BaseType(name,param.map(t => t.substitute(tvar,te)))
//  }
//}
///* typeExp -> typeExp */
//case class TMap(from: TypeExpr, to:TypeExpr) extends TypeExpr {
//  def substitute(tvar:TVar,te:TypeExpr):TMap = {
//    TMap(from.substitute(tvar,te),to.substitute(tvar,te))
//  }
//}
///* type variable */
//case class TVar(name:String) extends TypeExpr {
//  def occurs(te:TypeExpr):Boolean = te match {
//    case TUnit => false
//    case t@TVar(n) => this == t
//    case TMap(t1,t2) => this.occurs(t1) || this.occurs(t2)
//    case BaseType(name, param) => param.exists(t=> this.occurs(t))
//    case TOpt(t) => this.occurs(t)
//    case TEithers(f,o) => this.occurs(f) || o.exists(t=> this.occurs(t))
//    case TTuple(f,o) => this.occurs(f) || o.exists(t=> this.occurs(t))
//    case TProd(f,o) => this.occurs(f) || o.exists(t=> this.occurs(t))
//  }
//
//  def substitute(tvar:TVar,te:TypeExpr):TypeExpr = if (this == tvar) te else this
//}
//
///* Opt[typeExp] */
//case class TOpt(t:TypeExpr) extends TypeExpr {
//  def substitute(tvar: TVar, te: TypeExpr): TOpt = TOpt(t.substitute(tvar,te))
//}
//
/////* Either<typeExp,typeExpr> */
////case class TEither(l:TypeExpr,r:TypeExpr) extends TypeExpr {
////  def substitute(tvar: TVar, te: TypeExpr): TEither =
////    TEither(l.substitute(tvar,te),r.substitute(tvar,te))
////}
//
///* Eithers<typeExp,typeExpr*> */
//case class TEithers(first:TypeExpr,others:List[TypeExpr]) extends TypeExpr {
//  def substitute(tvar: TVar, te: TypeExpr): TEithers =
//    TEithers(first.substitute(tvar,te),others.map( _.substitute(tvar,te)))
//}
//
/////* Pair<typeExpr,typeExpr> */
////case class TPair(t1:TypeExpr,t2:TypeExpr) extends TypeExpr {
////  def substitute(tvar: TVar, te: TypeExpr): TPair =
////    TPair(t1.substitute(tvar,te),t2.substitute(tvar,te))
////}
//
///* Join<typeExpr,typeExpr*> */
//case class TTuple(first:TypeExpr, others:List[TypeExpr]) extends TypeExpr {
//  def substitute(tvar: TVar, te: TypeExpr): TTuple =
//    TTuple(first.substitute(tvar,te),others.map(_.substitute(tvar,te)))
//}
//
///* typeExpr x typeExpr  */
//case class TProd(first:TypeExpr, others:List[TypeExpr]) extends TypeExpr {
//  def substitute(tvar: TVar, te: TypeExpr): TProd =
//    TProd(first.substitute(tvar,te),others.map(_.substitute(tvar,te)))
//}
//
//case object TUnit extends TypeExpr {
//  def substitute(TVar: TVar,te:TypeExpr):TypeExpr = this
//}
