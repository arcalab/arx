package dsl.analysis.types

import dsl.backend.{Show, Simplify}
import dsl.common.TypeException

/**
  * Created by guillecledou on 2020-01-22
  */


object Destructor {

  def apply(ctx:Context,tExp:TExp):TExp = tExp match {
    case TBase(name, tParams) if ctx.adts.contains(name)=>
      val constructors = ctx.adts(name).constructors
      val res = constructors.map(c=>destruct(ctx,c.params)).foldRight[TExp](TUnit)(TTensor)
      Simplify(res)
    case _ => throw new TypeException(s"Only ground types can be destruct but ${Show(tExp)} found")
  }

  def expand(destr: TExp,ctx:Context):TExp = destr match {
    case TDestr(t) => apply(ctx,t)
    case _ => destr
  }

//  def expand(destr: TExp,tExp:TExp):TExp = destr match {
//    case TDestr(t) => tExp
//    case _ => destr
//  }

  private def destruct(ctx:Context,paramTypes:List[TExp]):TExp =  paramTypes match {
    case Nil => ctx.adts("Unit").tExp //TBase("Unit",List())
    case p::ps => TTensor(apply(ctx,p),destruct(ctx,ps))
  }

}
