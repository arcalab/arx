package dsl.analysis.types

import dsl.backend.{Show, Simplify}
import dsl.common.TypeException

/**
  * Created by guillecledou on 2020-01-22
  */


object Destructor {

  def apply(ctx:Context,tExp:TExp):TExp = tExp match {
    case TBase(name, tParams) if ctx.adts.contains(name)=>
      // get the type context entry
      val basetype:TypeEntry = ctx.adts(name)
      // get constructors of the type
      val constructors:List[ConstEntry] = basetype.constructors
      // get the type vars in the types definition (in order)
      val typeVars:List[TVar] = basetype.tExp.tParams.flatMap(p=> p.vars)
      val substitute = Substitution(typeVars.zip(tParams).toMap)
      // replace all type vars in the constructors definition by the known ones
      val substConstr:List[List[TExp]] = constructors.map(c=> c.params.map(substitute(_)))
      val res = substConstr.map(c=>destruct(ctx,c)).foldRight[TExp](TUnit)(TTensor)
      Simplify(res)
    case _ => throw new TypeException(s"Only ground types can be destruct but ${tExp} found")
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
    case _ => Simplify(paramTypes.foldRight[TExp](TUnit)(TTensor))
    //case p::ps => TTensor(apply(ctx,p),destruct(ctx,ps))
  }

}
