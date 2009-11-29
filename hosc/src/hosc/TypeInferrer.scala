package hosc

import EnrichedLambdaCalculus._
import TypeAlgebra._

case class TypeError(s: String) extends Exception(s)

class Subst(val map: Map[TypeVariable, Type]) extends (Type => Type) {
  
  def this() = this(Map())
  def compose(s: Subst) = new Subst(s.map.transform((k, v) => this(v)) ++ this.map)
  def excl(vs: List[TypeVariable]) = new Subst(map -- vs)
    
  def apply(t: Type): Type = t match {
    case tv : TypeVariable => map.getOrElse(tv, tv)
    case Arrow(t1, t2) => Arrow(this(t1), this(t2))
    case TypeConstructor(k, ts) => TypeConstructor(k, ts map this)
  }

  def extend(x: TypeVariable, t: Type) =
    if (tyvars(t) contains x)
      throw TypeError("recursive binding: " + x + " = " + t)
    else if (x == t) this
    else new Subst(Map(x -> t)) compose this
}

case class TypeScheme(genericVars: List[TypeVariable], t: Type) {
  def newInstance = (new Subst(Map(genericVars map {(_, newTyvar)}:_*))) (t)
  def nonGenericVars = tyvars(t) -- genericVars
  def sub(sub: Subst) = TypeScheme(genericVars, (sub excl genericVars) (t))
}

case class TypeEnv(map: Map[TypeVariable, TypeScheme]){
  def value(tv: TypeVariable) = map(tv)
  def install(tv: TypeVariable, ts: TypeScheme) = TypeEnv(map + {(tv, ts)})
  def nonGenericVars = map.values.toList flatMap {_.nonGenericVars}
  def sub(s: Subst) = TypeEnv(map transform {(k, v) => v.sub(s)})
}

object TypeInferrer {

  private def mgu(t: Type, u: Type, s: Subst): Subst = (t, u) match {
    case (a:TypeVariable, b: TypeVariable) if a == b =>
      s
    case (a:TypeVariable, _) if s(a) == a =>
      s.extend(a, u)
    case (a:TypeVariable, _) =>
      mgu(s(t), s(u), s)
    case (_, a:TypeVariable) =>
      mgu(u, t, s)
    case (Arrow(t1, t2), Arrow(u1, u2)) =>
      mgu(t1, u1, mgu(t2, u2, s))
    case (TypeConstructor(k1, ts), TypeConstructor(k2, us)) if (k1 == k2) =>
      (s /: (ts zip us)) ((s, tu) => mgu(tu._1, tu._2, s))
    case _ =>
      throw new TypeError("cannot unify " + s(t) + " with " + s(u))
  }
  
  private def mgu(ts: List[Pair[Type, Type]], s: Subst): Subst = {
    (s /: ts) {(s, p) => mgu(p._1, p._2, s)}
  }
  
}

import TypeInferrer._
class TypeInferrer(typeDefs: List[TypeConstructorDefinition]) {
  
  private val dataConstructors = Map(typeDefs flatMap {_.cons map {c => c.name -> c}}:_*)
  private val typeConstructorDefs = Map(typeDefs flatMap {td => td.cons map {_.name -> td} }:_*)
  
  def inferType(e: Expression): Type = {
    val v2scheme = {v: Variable => TypeVariable(v.name) -> TypeScheme(Nil, newTyvar)}
    val te = TypeEnv(Map(freeVars(e).toList map v2scheme :_*))
    tc(te, e)._2
  }
  
  def tc(te: TypeEnv, expr: Expression): (Subst, Type) = expr match {
    case v: Variable => 
      (new Subst(), te.value(TypeVariable(v.name)).newInstance)
    case a: Application => {
      val (sub1, type1s) = check(te, a.head :: a.arg :: Nil)
      val genericVar = newTyvar()
      val sub2 = mgu(type1s.head, Arrow(type1s.last, genericVar), sub1)
      (sub2, sub2(genericVar))
    }
    case l: LambdaAbstraction => checkLambda(te, l)
    case l: LetExpression => tcLet(te, l)
    case l: LetRecExpression => tcLetRec(te, l)
    case c: Constructor => tcCon(te, c)
    case c: CaseExpression => tcCase(te, c)
  }

  /** 
    *  When type-checking a lambda abstraction %x {e} wrt a given type environment te
    *  we propose that x:v, e:bt and associate with x new type scheme
    *  ts = ([], w).
    *  
    *  Then we infer the type t of a lambda body wrt extended type environment
    *  te + ts. We do it by constructing substitution which solves type constraints for bt.
    *
    *  Then %x {e}: v -> bt.
    * 
    *  @param te type environment
    *  @param lambda lambda abstraction to be checked
    *  @return a substitution that solves type constraints wrt environment  
    *          and a derived type for an lambda abstraction
    */
  private def checkLambda(te: TypeEnv, lambda: LambdaAbstraction): (Subst, Type) = {    
    val argSchematicType = newTyvar()
    val extendedEnv = te.install(TypeVariable(lambda.v.name), TypeScheme(Nil, argSchematicType)) 
    
    val (sub, typ) = tc(extendedEnv, lambda.t)
    (sub, Arrow(sub(argSchematicType), typ))
  }
    
  /** 
    *  When type-checking a let expression
    *  let x1 = e1, x2 = e2, ... , xn = en in e0 
    *  wrt a given type environment te
    *  we first of all check e1, e2, ..., en
    *  
    *  Then we update te with the information obtained by checking e1, e2, ...
    *  Suppose that after type checking of e1, e2, ..., en
    *  e1: et1, e2: et2, ..., en: etn, 
    *  
    *  We construct from et1, et2, ... etn corresponding type schemes s1, s2, ... (wrt to env)
    *
    *  We associate e1 with et1, etc. and update env with these associations 
    *
    *  Then we type check e0 wrt extended environment
    * 
    *  @param te type environment
    *  @param lambda lambda abstraction to be checked
    *  @return a substitution that solves type constraints wrt environment  
    *          and a derived type for an lambda abstraction
    */    
  private def tcLet(env: TypeEnv, let: LetExpression): (Subst, Type) = {      
    // check the rights sides of bindings
    val letExps = let.bs map {_._2}    
    val (sub1, type1s) = check(env, letExps)
    
    val tvs = let.bs map {p => TypeVariable(p._1.name)}
    val gamma1 = env.sub(sub1)
    val gamma2 = addDecls(gamma1, tvs, type1s)
    
    // check the let body wrt updated env
    val (sub2, type2) = tc(gamma2, let.expr)
    (sub1 compose sub2, type2)
  }
  
  /** 
    *  When type-checking a letrec expression
    *  letrec x1 = e1, x2 = e2, ... , xn = en in e0 
    *  wrt a given type environment te we perform following steps:
    *  
    *  1. Associate new type schemes with variables x1, x2, ..., xn.
    *     x1:t1, x2:t2, ..., xn:tn 
    *     These schemes have no schematic variables.  
    *  2. Type check the right-hand sides in env
    *     extended with type schemes for x1, x2,..
    *     It yields to substitution phi1 and types:
    *       e1: et1, e2: et2, ..., en: etn,
    *  3. Apply phi1 to t1, t2, ..., tn
    *     t1' = phi1(t1), t2' = phi1(t2), tn' = phi1(tn)
    *     Construct an extension of phi1 (phi2) that solves eqns:
    *     t1' = e1, ..., tn' = etn 
    *  4. Now types for definitions are inferred. We update env with this knowledge
    *     and infer type for letrec body wrt updated env.
    *
    *  @param te type environment
    *  @param lambda lambda abstraction to be checked
    *  @return a substitution that solves type constraints wrt environment  
    *          and a derived type for an lambda abstraction
    */  
  private def tcLetRec(env: TypeEnv, letrec: LetRecExpression): (Subst, Type) = {
    // TODO: refactor after Result is eliminated
    val (letRecVars, letRecRSides) = List.unzip(letrec.bs) 
    
    val xs = letRecVars map {x => TypeVariable(x.name)}
    val schemes = letRecVars map {x => TypeScheme(Nil, newTyvar)}
    
    // check right hand sides in extended env
    val (sub1, type1s) = check(TypeEnv(env.map ++ (xs zip schemes)), letRecRSides)
    
    val lSideTypes = (schemes map {s => s.sub(sub1)}) map {_.t}   
    // unifying left and right sides
    val phi2 = mgu(lSideTypes zip type1s, sub1)    
    // apply sub2 to env and also extend result with the derived type schemes for xs
    val extEnv = addDecls(env.sub(phi2), xs, lSideTypes map phi2)
    // type check the letrec body wrt updated env
    val (sub2, type2) = tc(extEnv, letrec.expr)
    // construct result
    (phi2 compose sub2, type2)
  }
  
  /** 
    *  When inferring type for a constructor C e1 .. en wrt a given type environment
    *  te we first of all infer e1 .. en that results into substitution phi1
    *  and types et1 .. etn such that e1:et1 .. en:etn
    * 
    *  Then we construct an extension of phi1 (phi2) that unifies inferred types of constructor
    *  arguments with the declared ones.
    *
    *  @param te type environment
    *  @param c Constructor to be checked
    *  @return a substitution that solves type constraints wrt environment  
    *          and a derived type for a constructor
    */  
  private def tcCon(te: TypeEnv, c: Constructor): (Subst, Type) = {
    // infer constructor arguments
    val (sub1, type1s) = check(te, c.args)
    
    // construct equqations saying that declared types equal to inferred ones
    val conDef = typeConstructorDefs(c.name)
    val typeParams = conDef.args
    // just substitution refreshing parametric variables
    val fSub = (new Subst() /: typeParams) ((sub, tv) => sub.extend(tv, newTyvar()))    
    val freshedDcArgs: List[Type] = dataConstructors(c.name).args map fSub    
    val typeEqns = freshedDcArgs zip type1s
    
    // solve equations and construct answer
    val sub2 = mgu(typeEqns, sub1)
    val cvars = typeParams map (sub2 compose fSub)
    (sub2, TypeConstructor(conDef.name, cvars))
  }
  
  /** 
    *  Updates a type environment te with the type schemes for xs derived from ts.
    *  
    *  Each variable that is schematic in new scheme is schematic in te.
    *
    *  So this method extends te preserving schematic variables.
    * 
    *  @param te type environment
    *  @param xs type variable
    *  @param ts types associated with xs
    *  @return te extended with schemes for xs
    */   
  private def addDecls(te: TypeEnv, xs: List[TypeVariable], ts: List[Type]): TypeEnv = {
    val nonGenericVars = te.nonGenericVars
    val schemes = ts map {createTypeScheme(nonGenericVars, _)}
    TypeEnv(te.map ++ (xs zip schemes))
  } 
  
  /** 
    *  Generates a type scheme for a type t given a knowledge about 
    *  non schematic variables in a type.
    * 
    *  @param unknowns list of non schematic variables of type t
    *  @param t type for which type scheme should be constructed
    *  @return a type scheme ts such that ts.unknowns = unknowns
    *  
    */
  private def createTypeScheme(unknowns: List[TypeVariable], t: Type): TypeScheme = {
    // schematic vars
    val scvs = tyvars(t).removeDuplicates -- unknowns
    // map from schematic vars to new vars
    val al = scvs map (tv => (tv, newTyvar))
    // substitution of new vars instead of schematic vars
    val sub = (new Subst() /: al)((sub, tv) => sub.extend(tv._1, tv._2))
    // schematic variables are freshed in t1
    val t1 = sub(t)
    // type scheme where schematic vars are freshed
    TypeScheme(al map {_._2}, t1)
  }
  
  /** 
    *  When type-checking case expression
    *  case sel e0 of {b1 ... bn}
    *  we consider b1 .. bn as pattern-matching lambda abstraction
    *  
    *  First of all we infer types for b1 .. bn:
    *  b1:bt1 .. bn:bt1
    *  Then we infer type bt for case body by solving following exqns:
    *  bt = bt1 .. bt = btn
    *  As result we get bt and substitution phi
    *
    *  Then we infer type for sel wrt phi(env) 
    *  sel:selT.
    *  
    *  After selT and bT are found we can write additional constraint
    *  bT = selT -> caseT, where case ...:caseT
    *
    *  Then we try to construct an extension of phi (phi') which satisfies this additional
    *  constraint. We do it by unifying t1 with t2 -> t.
    *  Then result will be (phi', phi'(bT))
    *  
    *  @param te type environment
    *  @param caseExp case expression to be checked
    *  @return a substitution that solves type constraints wrt environment  
    *          and a derived type for a case expression
    */  
  private def tcCase(te: TypeEnv, caseExp: CaseExpression): (Subst, Type) = {
    val (sub1, type1) = tcCaseRaw(te, caseExp)
    val (sub2, type2) = tc(te.sub(sub1), caseExp.selector)
    val Arrow(selType, branchBodyType) = type1.asInstanceOf[Arrow]
    val sub = mgu(selType, type2, sub2 compose sub1)
    (sub, sub(branchBodyType))
  }
  
  // deals with branch as with pattern-matching lambda abstraction
  def tcBranch(te: TypeEnv, b: Branch): (Subst, Type) = {
    
    val cd = typeConstructorDefs(b.pattern.name)
    val dc = dataConstructors(b.pattern.name)
    
    val originalTvars = cd.args
    val s = (new Subst() /: originalTvars) ((sub, tv) => sub.extend(tv, newTyvar()))    
    val freshDcArgs: List[Type] = dc.args map s
    
    val tcon = s(TypeConstructor(cd.name, cd.args))
    
    var te1 = te
    for ((patternVar, argType) <- b.pattern.args zip freshDcArgs){
      te1 = te1.install(TypeVariable(patternVar.name), TypeScheme(Nil, argType))
    }
    
    val (sub3, type3) = tc(te1, b.term)
    (sub3, Arrow(sub3(tcon), type3))
  }
  
  private def tcCaseRaw(te: TypeEnv, caseExp: CaseExpression): (Subst, Type) = {
    val (sub1, type1s) = tcBranches(te, caseExp.branches)    
    val tv = newTyvar
    val pairs = type1s map {(tv, _)}    
    val sub2 = mgu(pairs, sub1)
    (sub2, sub2(tv))
  }
  
  private def tcBranches(environment: TypeEnv, expressions: List[Branch]): (Subst, List[Type]) = expressions match {
    case Nil => (new Subst(), Nil) 
    case e :: es => {
      val (sub1, type1) = tcBranch(environment, e)
      val (sub2, types2) = tcBranches(environment.sub(sub1), es)
      (sub2 compose sub1, sub2(type1) :: types2)
    }
  }
  
  private def check(environment: TypeEnv, expressions: List[Expression]): (Subst, List[Type]) = expressions match {
    case Nil => (new Subst(), Nil) 
    case e :: es => {
      val (sub1, type1) = tc(environment, e)
      val (sub2, types2) = check(environment sub sub1, es)
      (sub2 compose sub1, sub2(type1) :: types2)
    }
  }
  
}