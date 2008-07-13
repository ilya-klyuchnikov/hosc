package hosc

import EnrichedLambdaCalculus._

object TypeInferrer {
  private var n: Int = 0
  def newTyvar(): TypeVariable = { n += 1; TypeVariable("$$" + n) }
  case class TypeError(s: String) extends Exception(s) {}

  // substitutions
  class Subst extends Function1[Type, Type] {
    def baseApply(x: TypeVariable): Type = x
    
    def apply(t: Type): Type = t match {
      case tv : TypeVariable => baseApply(tv)
      case Arrow(t1, t2) => Arrow(apply(t1), apply(t2))
      case TypeConstructor(k, ts) => TypeConstructor(k, ts map apply)
    }
    
    def extend(x: TypeVariable, t: Type): Subst =
      if (x == t) {
        this 
      } else {
        tyvars(t) contains x match {
          case false =>  new Subst {
            val cs = delta(x, t) compose Subst.this
            override def apply(t: Type) = cs.apply(t)
            override def toString = cs.toString
          }
          case true => 
            throw new TypeError("recursive binding: " + x + " = " + t)
        }
      }
      
    
    def compose(z: Subst): Subst = new Subst {
      override def apply(t: Type) = Subst.this.apply(z.apply(t))
      override def toString = "{" + Subst.this + " * " + z + "}"
    }
    
    def exclude(vs: List[TypeVariable]) = new Subst {
      override def apply(y: Type): Type = if (vs contains y) y else Subst.this.apply(y)
      override def toString = "{" + Subst.this + " / (" + vs.mkString(", ") + ")}"
    }
  } 

  val emptySubst = new Subst {
    override def baseApply(t: TypeVariable): Type = t
    override def toString = ""
  }
  
  private def delta(x: TypeVariable, t: Type) = new Subst {
    override def baseApply(tv: TypeVariable): Type = if (x == tv) t else tv
    override def toString = x + "=" + t + ";"
  }
  
  // type schemes
  case class TypeScheme(schematicVars: List[TypeVariable], t: Type) {
    // returns the type contained in the scheme 
    // after all schematic type variables have been renamed to fresh variables    
    def newInstance(): Type =
      (emptySubst /: schematicVars) ((sub, tv) => sub.extend(tv, newTyvar())) (t)
    
    def unknownVars = tyvars(t) -- schematicVars
    
    def sub(sub: Subst) = TypeScheme(schematicVars, (sub exclude schematicVars) (t))
  }
  
  case class TypeEnv(al: List[Pair[TypeVariable, TypeScheme]]){
    def value(tv: TypeVariable) = al find(_._1 == tv) match {
      case Some(x) => x._2
      case None => throw new TypeError("undefined: " + tv)
    }
    def dom = al map (_._1)
    def mg = for (a <- dom) yield value(a)
    def install(tv: TypeVariable, ts: TypeScheme) = TypeEnv((tv, ts) :: al)
    def unknownsVars = (mg :\ List[TypeVariable]()){(ts, l) => (l ::: ts.unknownVars )}
    // sub_te
    def sub(s: Subst) = TypeEnv(al map {p => (p._1, p._2.sub(s))})
  }
  
  private def tyvars(t: Type): List[TypeVariable] = t match {
    case tv @ TypeVariable(a) => List(tv)
    case Arrow(t1, t2) => tyvars(t1) union tyvars(t2)
    case TypeConstructor(k, ts) => (List[TypeVariable]() /: ts) ((tvs, t) => tvs union tyvars(t))
  }  

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
  
  case class Result(s: Subst, t: Type) {
    override def toString = t + "\n" + s
  }
  case class ResultL(s: Subst, ts: List[Type])
  
  private def equal(type1: Type, type2: Type): Boolean = {
    def equal1(tp1: Type, tp2: Type): Boolean = {
      var map1to2 = scala.collection.mutable.Map[TypeVariable, TypeVariable]()
      def test(t1: Type, t2: Type): Boolean = (t1, t2) match {
        case (v1: TypeVariable, v2: TypeVariable) => map1to2.get(v1) match {
          case None => map1to2(v1) = v2; true
          case Some(v) if v2 == v => true
          case _ => false
        }
        case (TypeConstructor(n1, a1), TypeConstructor(n2, a2)) =>
          n1 == n2 && a1.length == a2.length && ((a1 zip a2) forall {pair => test(pair._1, pair._2)})
        case (Arrow(a1, v1), Arrow(a2, v2)) => test(a1, a1) && test(v1, v2)
        case _ => false
      }
      test(tp1, tp2)
    }
    equal1(type1, type2) && equal(type2, type1)
  }
}

import TypeInferrer._
class TypeInferrer(typeDefs: List[TypeConstructorDefinition]) {
  
  private val cnameDC = Map(typeDefs flatMap {_.cons map {c => c.name -> c}}:_*)
  private val cnameTD = Map(typeDefs flatMap {td => td.cons map {_.name -> td} }:_*)
  
  def inferType(expr: Expression) = {
    var te = TypeEnv(Nil)
    for (v <- getFreeVars(expr)){
      val nv = newTyvar()
      val ts = TypeScheme(Nil, nv)
      val lv = TypeVariable(v.name)
      te = te.install(lv, ts)
    }
    tc(te, expr).t
  }
  
  private def getFreeVars(expr: Expression): Set[Variable] = expr match {
    case v: Variable => Set(v)
    case Constructor(_, args) => (Set[Variable]() /: args) {(vs, term) => vs ++ getFreeVars(term)}
    case LambdaAbstraction(x, term) => getFreeVars(term) - x
    case Application(head, arg) => getFreeVars(head) ++ getFreeVars(arg)
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
    Result(emptySubst, te.value(TypeVariable(v.name)).newInstance())
  
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
    val letRecVars = letrec.bs map {_._1}
    
    val xs = letRecVars map {x => TypeVariable(x.name)}
    val schemes = letRecVars map {x => TypeScheme(Nil, newTyvar)}
    
    // check right hand sides in extended env
    val rl = check(TypeEnv((xs zip schemes) :::  env.al), letrec.bs map {_._2})
    
    val phi1 = rl.s
    val rSideTypes = rl.ts
    
    val lSideTypes = (schemes map {s => s.sub(phi1)}) map {_.t}   
    // unifying left and right sides
    val phi2 = mgu(lSideTypes zip rSideTypes, phi1)    
    // apply sub2 to env and also extend result with the derived type schemes for xs
    val extEnv = addDecls(env.sub(phi2), xs, lSideTypes)
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
    val fSub = (emptySubst /: typeParams) ((sub, tv) => sub.extend(tv, newTyvar()))    
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
    val unknowns = te.unknownsVars
    val schemes = ts map {createTypeScheme(unknowns, _)}
    TypeEnv((xs zip schemes) ::: te.al)
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
    val sub = (emptySubst /: al)((sub, tv) => sub.extend(tv._1, tv._2))
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
    val appSchematicType = newTyvar()
    val sub = mgu(r1.t, Arrow(r2.t, appSchematicType), r2.s compose r1.s)
    Result(sub, sub(appSchematicType))
  }
  
  // deals with branch as with pattern-matching lambda abstraction
  def tcBranch(te: TypeEnv, b: Branch): Result = {
    
    val cd = cnameTD(b.pattern.name)
    val dc = cnameDC(b.pattern.name)
    
    val originalTvars = cd.args
    val s = (emptySubst /: originalTvars) ((sub, tv) => sub.extend(tv, newTyvar()))    
    val freshDcArgs: List[Type] = dc.args map s
    
    val tcon = s(TypeConstructor(cd.name, cd.args))
    
    var te1 = te
    for ((patternVar, argType) <- b.pattern.args zip freshDcArgs){
      te1 = te1.install(TypeVariable(patternVar.name), TypeScheme(Nil, argType))
    }
    
    val res = tc(te1, b.term)
    Result(res.s, Arrow(res.s(tcon), res.t))
  }
  
  //type-checking of list of branches
  private def tclb(tes: TypeEnv, tss: List[Branch]):ResultL = {
    def tcl0(te: TypeEnv, ts: List[Branch]): ResultL = ts match {
      case Nil => ResultL(emptySubst, Nil) 
      case e :: es => tcl1(te, es, tcBranch(te, e))
    }

    def tcl1(te: TypeEnv, es: List[Branch], r: Result) = {
      val gamma = te.sub(r.s)
      tcl2(r.s, r.t, tcl0(gamma, es))
    }

    def tcl2(phi: Subst, t: Type, r: ResultL) = ResultL(r.s compose phi, r.s(t) :: r.ts)

    tcl0(tes,tss)
  }
  
  def tcCaseRaw(te: TypeEnv, caseExp: CaseExpression): Result = {
    val r = tclb(te, caseExp.branches)    
    val tv = newTyvar
    val pairs = r.ts map {(tv, _)}    
    val s = mgu(pairs, r.s)
    Result(s, s(tv))
  }
  
  // type-checking of list of expressions
  private def check(environment: TypeEnv, expressions: List[Expression]): ResultL = expressions match {
    case Nil => ResultL(emptySubst, Nil) 
    case e :: es => {
      val r1 = tc(environment, e)
      val rs = check(environment sub r1.s, es)
      ResultL(rs.s compose r1.s, rs.s(r1.t) :: rs.ts)
    }
  }
  
}