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
    check(te, e)._2
  }
  
  private def check(te: TypeEnv, expr: Expression): (Subst, Type) = expr match {
    case Variable(name) => 
      (new Subst(), te.value(TypeVariable(name)).newInstance)
    case Application(h, a) => {
      val (sub1, List(type_h, type_a)) = check(te, List(h, a))
      val genVar = newTyvar
      val sub2 = mgu(type_h, Arrow(type_a, genVar), sub1)
      (sub2, sub2(genVar))
    }
    case LambdaAbstraction(v, body) => {
      val genVar = newTyvar
      val extendedEnv = te.install(TypeVariable(v.name), TypeScheme(Nil, genVar)) 
      val (sub, type_body) = check(extendedEnv, body)
      (sub, Arrow(sub(genVar), type_body))
    }
    case Constructor(name, args) => {
      val (sub1, type1s) = check(te, args)
      val conDef = typeConstructorDefs(name)
      val typeParams = conDef.args
      val freshSub = new Subst(Map(typeParams map {(_, newTyvar)}:_*))    
      val freshDataConArgs = dataConstructors(name).args map freshSub
      val sub2 = mgu(freshDataConArgs zip type1s, sub1)
      (sub2, TypeConstructor(conDef.name, typeParams map (sub2 compose freshSub)))
    }
    case l: LetExpression => tcLet(te, l)
    case l: LetRecExpression => tcLetRec(te, l)
    case c: CaseExpression => tcCase(te, c)
  }
  
  private def check(environment: TypeEnv, expressions: List[Expression]): (Subst, List[Type]) = expressions match {
    case Nil => (new Subst(), Nil) 
    case e :: es => {
      val (sub1, type1) = check(environment, e)
      val (sub2, types2) = check(environment sub sub1, es)
      (sub2 compose sub1, sub2(type1) :: types2)
    }
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
    val env1 = env.sub(sub1)
    val env2 = addDecls(env1, let.bs map {p => TypeVariable(p._1.name)}, type1s)
    val (sub2, type2) = check(env2, let.expr)
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
    val (sub2, type2) = check(extEnv, letrec.expr)
    // construct result
    (phi2 compose sub2, type2)
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
    val schemes = ts map {createTypeScheme(te.nonGenericVars, _)}
    TypeEnv(te.map ++ (xs zip schemes))
  } 
  
  private def createTypeScheme(nonGenericVars: List[TypeVariable], t: Type): TypeScheme = {
    val genericVars = tyvars(t) -- nonGenericVars
    val map = Map(genericVars map {(_, newTyvar)}:_*)
    TypeScheme(map.values.toList, new Subst(map)(t))
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
    val (sub2, type2) = check(te.sub(sub1), caseExp.selector)
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
    
    val (sub3, type3) = check(te1, b.term)
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
  
}