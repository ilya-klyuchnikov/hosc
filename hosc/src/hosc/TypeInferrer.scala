package hosc

import EnrichedLambdaCalculus._
import TypeAlgebra._

case class TypeError(s: String) extends Exception(s)
case class Result(s: Subst, t: Type)
case class ResultL(s: Subst, ts: List[Type])

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
  def newInstance() = (new Subst(Map(genericVars map {(_, newTyvar)}:_*))) (t)
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
  
  private val cnameDC = Map(typeDefs flatMap {_.cons map {c => c.name -> c}}:_*)
  private val cnameTD = Map(typeDefs flatMap {td => td.cons map {_.name -> td} }:_*)
  
  // the 'main' interface method
  def inferType(expr: Expression) = {
    var te = TypeEnv(Map())
    for (v <- getFreeVars(expr)){
      val nv = newTyvar()
      val ts = TypeScheme(Nil, nv)
      val lv = TypeVariable(v.name)
      te = te.install(lv, ts)
    }
    val r =tc(te, expr).t
    r
  }
  
  private def getFreeVars(expr: Expression): Set[Variable] = expr match {
    case v: Variable => Set(v)
    case Constructor(_, args) => (Set[Variable]() /: args) {(vs, term) => vs ++ getFreeVars(term)}
    case LambdaAbstraction(x, term) => getFreeVars(term) - x
    case Application(head, arg) => 
      getFreeVars(head) ++ getFreeVars(arg)
    case CaseExpression(sel, bs) => 
      getFreeVars(sel) ++ (Set[Variable]() /: bs) {(vs, b) => vs ++ (getFreeVars(b.term) -- b.pattern.args)}
    case LetRecExpression(bs, expr) => 
      ((getFreeVars(expr) /: bs) {(vs, b) => vs ++ getFreeVars(b._2)}) -- (bs map {_._1})
    case LetExpression(bs, expr) => 
      ((getFreeVars(expr) /: bs) {(vs, b) => vs ++ getFreeVars(b._2)}) -- (bs map {_._1})
  }
  
  def tc(te: TypeEnv, expr: Expression): Result = expr match {
    case v: Variable => checkVar(te, v)
    case a: Application => checkApp(te, a)
    case l: LambdaAbstraction => checkLambda(te, l)
    case l: LetExpression => tcLet(te, l)
    case l: LetRecExpression => tcLetRec(te, l)
    case c: Constructor => tcCon(te, c)
    case c: CaseExpression => tcCase(te, c)
  }
  
  /** 
    *  When type-checking a variable v wrt a given type environment
    *  te we lookup the type scheme associated with that variable by te
    *  and return an identity substitution and schematic type (part of type scheme) 
    *  where all schematic variables are refreshed.  
    * 
    *  @param te type environment
    *  @param v variable to be checked
    *  @return and an identity substitution and schematic type  
    *          in which all schematic variables are replaced
    *          by fresh type variables
    */
  private def checkVar(te: TypeEnv, v: Variable): Result =
    Result(new Subst(), te.value(TypeVariable(v.name)).newInstance())
  
  /** 
    *  When type-checking an application (e1 e2) wrt a given type environment
    *  te we first of all construct a substitution phi which solves 
    *  the type constraints on e1 and e2 together.
    *  
    *  During constructing phi some types t1 and t2 are derived for e1 and e2:
    *  e1 : t1,
    *  e2 : t2.
    *  
    *  Let t be a type of (e1 e2). That is:
    *  (e1 e2) : t.
    *  This results in additional constraint (equation):
    *  t1 = t2 -> t.
    * 
    *  Then we try to construct an extension of phi which satisfies this additional
    *  constraint. We do it by unifying t1 with t2 -> t.
    *
    *  @param te type environment
    *  @param a application to be checked
    *  @return a substitution that solves type constraints wrt environment  
    *          and a derived type for an application
    */ 
  private def checkApp(te: TypeEnv, a: Application): Result = {  
    val r = check(te, a.head :: a.arg :: Nil)
    val appSchematicType = newTyvar()
    val sub = mgu(r.ts.head, Arrow(r.ts.last, appSchematicType), r.s)
    Result(sub, sub(appSchematicType))
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
  private def checkLambda(te: TypeEnv, lambda: LambdaAbstraction): Result = {    
    val argSchematicType = newTyvar()
    val extendedEnv = te.install(TypeVariable(lambda.v.name), TypeScheme(Nil, argSchematicType)) 
    
    val r = tc(extendedEnv, lambda.t)
    Result(r.s, Arrow(r.s(argSchematicType), r.t))
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
  private def tcLet(env: TypeEnv, let: LetExpression): Result = {      
    // check the rights sides of bindings
    val letExps = let.bs map {_._2}    
    val r = check(env, letExps)
    
    val tvs = let.bs map {p => TypeVariable(p._1.name)}
    val gamma1 = env.sub(r.s)
    val gamma2 = addDecls(gamma1, tvs, r.ts)
    
    // check the let body wrt updated env
    val r2 = tc(gamma2, let.expr)
    Result(r.s compose r2.s, r2.t)
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
  private def tcLetRec(env: TypeEnv, letrec: LetRecExpression): Result = {
    // TODO: refactor after Result is eliminated
    val (letRecVars, letRecRSides) = List.unzip(letrec.bs) 
    
    val xs = letRecVars map {x => TypeVariable(x.name)}
    val schemes = letRecVars map {x => TypeScheme(Nil, newTyvar)}
    
    // check right hand sides in extended env
    val rl = check(TypeEnv(env.map ++ (xs zip schemes)), letRecRSides)
    
    val phi1 = rl.s
    val rSideTypes = rl.ts
    
    val lSideTypes = (schemes map {s => s.sub(phi1)}) map {_.t}   
    // unifying left and right sides
    val phi2 = mgu(lSideTypes zip rSideTypes, phi1)    
    // apply sub2 to env and also extend result with the derived type schemes for xs
    val extEnv = addDecls(env.sub(phi2), xs, lSideTypes map phi2)
    // type check the letrec body wrt updated env
    val r = tc(extEnv, letrec.expr)
    // construct result
    Result(phi2 compose r.s, r.t)
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
  private def tcCon(te: TypeEnv, c: Constructor): Result = {
    // infer constructor arguments
    val rl = check(te, c.args)
    
    // construct equqations saying that declared types equal to inferred ones
    val conDef = cnameTD(c.name)
    val typeParams = conDef.args
    // just substitution refreshing parametric variables
    val fSub = (new Subst() /: typeParams) ((sub, tv) => sub.extend(tv, newTyvar()))    
    val freshedDcArgs: List[Type] = cnameDC(c.name).args map fSub    
    val typeEqns = freshedDcArgs zip rl.ts
    
    // solve equations and construct answer
    val sub2 = mgu(typeEqns, rl.s)
    val cvars = typeParams map (sub2 compose fSub)
    Result(sub2, TypeConstructor(conDef.name, cvars))
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
  private def tcCase(te: TypeEnv, caseExp: CaseExpression): Result = {
    val r1 = tcCaseRaw(te, caseExp)
    val r2 = tc(te.sub(r1.s), caseExp.selector)
    val Arrow(selType, branchBodyType) = r1.t.asInstanceOf[Arrow]
    val sub = mgu(selType, r2.t, r2.s compose r1.s)
    Result(sub, sub(branchBodyType))
  }
  
  // deals with branch as with pattern-matching lambda abstraction
  def tcBranch(te: TypeEnv, b: Branch): Result = {
    
    val cd = cnameTD(b.pattern.name)
    val dc = cnameDC(b.pattern.name)
    
    val originalTvars = cd.args
    val s = (new Subst() /: originalTvars) ((sub, tv) => sub.extend(tv, newTyvar()))    
    val freshDcArgs: List[Type] = dc.args map s
    
    val tcon = s(TypeConstructor(cd.name, cd.args))
    
    var te1 = te
    for ((patternVar, argType) <- b.pattern.args zip freshDcArgs){
      te1 = te1.install(TypeVariable(patternVar.name), TypeScheme(Nil, argType))
    }
    
    val res = tc(te1, b.term)
    Result(res.s, Arrow(res.s(tcon), res.t))
  }
  
  private def tcCaseRaw(te: TypeEnv, caseExp: CaseExpression): Result = {
    val r = tcBranches(te, caseExp.branches)    
    val tv = newTyvar
    val pairs = r.ts map {(tv, _)}    
    val s = mgu(pairs, r.s)
    Result(s, s(tv))
  }
  
  // type-checking of list of branches
  private def tcBranches(environment: TypeEnv, expressions: List[Branch]): ResultL = expressions match {
    case Nil => ResultL(new Subst(), Nil) 
    case e :: es => {
      val r1 = tcBranch(environment, e)
      val rs = tcBranches(environment.sub(r1.s), es)
      ResultL(rs.s compose r1.s, rs.s(r1.t) :: rs.ts)
    }
  }
  
  // type-checking of list of expressions
  private def check(environment: TypeEnv, expressions: List[Expression]): ResultL = expressions match {
    case Nil => ResultL(new Subst(), Nil) 
    case e :: es => {
      val r1 = tc(environment, e)
      val rs = check(environment sub r1.s, es)
      ResultL(rs.s compose r1.s, rs.s(r1.t) :: rs.ts)
    }
  }
  
}