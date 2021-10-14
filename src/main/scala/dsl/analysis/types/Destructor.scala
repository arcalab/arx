package dsl.analysis.types

import dsl.analysis.types.TProgram.TBlock
import dsl.backend.{Show, Simplify}
import dsl.common.TypeException

/**
  * Created by guillecledou on 2020-01-22
  */


object Destructor {

  def apply(ctx:Context,tExp:TExp):TExp = tExp match {
    case TBase(name, tParams) if ctx.hasType(name)=>
      // get the type context entry
      val basetype:TypeEntry = ctx.getType(name)
      // get constructors of the type
      val constructors:List[ConstEntry] = basetype.constructors.map(ctx.getConst)
      // get the type vars in the types definition (in order)
      val typeVars:List[TVar] = basetype.tExp.tParams.flatMap(p=> p.vars)
      val substitute = Substitution(typeVars.zip(tParams).toMap)
      // replace all type vars in the constructors definition by the known ones
      val substConstr:List[List[TExp]] = constructors.map(c=> c.paramsType.map(substitute(_)))
      val res = substConstr.map(c=>destruct(ctx,c)).foldRight[TExp](TUnit)(TTensor.apply)
      Simplify(res)
    case _ => throw new TypeException(s"Only ground types can be destruct but ${tExp} found")
  }

  def expand(destr: TExp,ctx:Context):TExp = destr match {
    case TDestr(t) => apply(ctx,t)
    case TTensor(t1,t2) => TTensor(expand(t1,ctx),expand(t2,ctx))
    case TFun(tin,tou) => TFun(expand(tin,ctx),expand(tou,ctx))
    case _ => destr
  }

  private def destruct(ctx:Context,paramTypes:List[TExp]):TExp =  paramTypes match {
    case Nil =>
      if (!ctx.hasType("Unit"))
        throw new TypeException(s"Stream fun build requires Unit type. Try 'import Types.Unit'")
      else ctx.getType("Unit").tExp //TBase("Unit",List())
    case _ => Simplify(paramTypes.foldRight[TExp](TUnit)(TTensor.apply))
    //case p::ps => TTensor(apply(ctx,p),destruct(ctx,ps))
  }

}
