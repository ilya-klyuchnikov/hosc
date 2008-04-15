package hosc

import HLanguage._
import GraphAnalysis._

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
    
    def extend(x: TypeVariable, t: Type): Subst = tyvars(t) contains x match {
      case false =>  new Subst {
        val cs = delta(x, t) compose Subst.this
        override def apply(t: Type) = cs.apply(t)
        override def toString = cs.toString
      }
      case true => 
        throw new TypeError("recursive binding: " + x + " = " + t)
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
  
  def delta(x: TypeVariable, t: Type) = new Subst {
    override def baseApply(tv: TypeVariable): Type = if (x == tv) t else tv
    override def toString = x + "=" + t + ";"
  }
  
  // type schemes
  case class TypeScheme(tvars: List[TypeVariable], t: Type) {
    // returns the type contained in the scheme 
    // after all schematic type variables have been renamed to fresh variables    
    def newInstance: Type =
      (emptySubst /: tvars) ((sub, tv) => sub.extend(tv, newTyvar())) (t)
    
    def unknownVars = tyvars(t) diff tvars
    
    def sub(s: Subst) = TypeScheme(tvars, (s exclude tvars) (t))
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
  
  def tyvars(t: Type): List[TypeVariable] = t match {
    case tv @ TypeVariable(a) => List(tv)
    case Arrow(t1, t2) => tyvars(t1) union tyvars(t2)
    case TypeConstructor(k, ts) => (List[TypeVariable]() /: ts) ((tvs, t) => tvs union tyvars(t))
  }  

  def mgu(t: Type, u: Type, s: Subst): Subst = (t, u) match {
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
  
  def mguL(ts: List[Pair[Type, Type]], s: Subst) = {
    (s /: ts) {(s, p) => mgu(p._1, p._2, s)}
  }
  
  case class Result(s: Subst, t: Type) {
    override def toString = t + "\n" + s
  }
  case class ResultL(s: Subst, ts: List[Type])
  
  def equal(type1: Type, type2: Type): Boolean = {
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
class TypeInferrer(p: Program) {
  
  def tc(te: TypeEnv, expr: Expression): Result = expr match {
    case v: Variable => tcVar(te, v)
    case a: Application => tcApp(te, a)
    case l: LambdaAbstraction => tcLambda(te, l)
    case l: LetExpression => tcLet(te, l)
    case l: LetRecExpression => tcLetRec(te, l)
    case c: Constructor => tcCon(te, c)
    case c: CaseExpression => tcCase(te, c)
  }
  
  def tcVar(te: TypeEnv, v: Variable): Result = {
    val tv = new TypeVariable(v.name)
    val ts = te.value(tv)
    Result(emptySubst, ts.newInstance)
  }
  
  def tcApp(t: TypeEnv, a: Application): Result = {
    def tcApp0(te: TypeEnv, app: Application): Result = 
      tcApp1(newTyvar(), tcl(te, List(app.head, app.arg)))
    
    def tcApp1(tv: TypeVariable, r: ResultL) =
      tcApp2(tv, mgu(r.ts.head, Arrow(r.ts.last, tv), r.s))
    
    def tcApp2(tv: TypeVariable, s: Subst) = 
      Result(s, s(tv))
      
    val r = tcl(t, List(a.head, a.arg))
    val tv = newTyvar
    val s = mgu(r.ts.head, Arrow(r.ts.last, tv), r.s)
    Result(s, s(tv))
  }
  
  def tcCase(te: TypeEnv, caseExp: CaseExpression): Result = {    
    val r1 = tcCaseRaw(te, caseExp)
    //val s = r1.s
    val r2 = tc(te, caseExp.selector)
    
    val arr = r1.t.asInstanceOf[Arrow]
   
    val s = mgu(arr.t1, r2.t, emptySubst)
    
    Result(s compose r1.s, s(arr.t2))
  }
  
  def tcCon(te: TypeEnv, c: Constructor): Result = {
    val cd = p.getTypeDefinitionForDC(c.name).get
    
    val originalTvars = cd.args    
    val dc = p.getDataConstructor(c.name).get 
    val s = (emptySubst /: originalTvars) ((sub, tv) => sub.extend(tv, newTyvar()))
    
    val freshDcArgs: List[Type] = dc.args map s
    val rl = tcl(te, c.args)
    val toUnify = freshDcArgs zip rl.ts
    val sub = mguL(toUnify, rl.s)
    val cvars = originalTvars map (sub compose s)
    Result(sub, TypeConstructor(cd.name, cvars))
  }
  
  def tcLambda(te: TypeEnv, l: LambdaAbstraction): Result = {    
    def tcLambda1(v: TypeVariable, r: Result) = Result(r.s, Arrow(r.s(v), r.t))
    
    val nv = newTyvar()
    val ts = TypeScheme(Nil, nv)
    val lv = TypeVariable(l.v.name)
    val te1 = te.install(lv, ts) 
    
    tcLambda1(nv, tc(te1, l.t))
  }
  
  def tcBranch(te: TypeEnv, b: Branch): Result = {
    
    val cd = p.getTypeDefinitionForDC(b.pattern.name).get
    val dc = p.getDataConstructor(b.pattern.name).get
    
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
  def tclb(tes: TypeEnv, tss: List[Branch]) = {
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
    val s = mguL(pairs, r.s)
    Result(s, s(tv))
  }
  
  
  
  //calculates schematic vars of t (given unknowns);
  // creates type scheme where schematic vars are freshed 
  def genBar(unknowns: List[TypeVariable], t: Type) = {
    // schematic vars
    val scvs = tyvars(t).removeDuplicates diff unknowns
    // map from schematic vars to new vars
    val al = scvs map (tv => (tv, newTyvar))
    // substitution of new vars instead of schematic vars
    val sub = (emptySubst /: al)((sub, tv) => sub.extend(tv._1, tv._2))
    // schematic variables are freshed in t1
    val t1 = sub(t)
    // type scheme where schematic vars are freshed
    TypeScheme(al map {_._2}, t1)
  }
  
  def addDecls(te: TypeEnv, vs: List[TypeVariable], ts: List[Type]) = {
    val unknowns = te.unknownsVars
    val schemes = ts map {genBar(unknowns, _)}
    TypeEnv((vs zip schemes) ::: te.al)
  }
  
  def tcLet(env: TypeEnv, l: LetExpression): Result = {
    
    def tcLet1(te: TypeEnv, xs: List[TypeVariable], e: Expression, r: ResultL) = {
      val gamma1 = te.sub(r.s)
      val gamma2 = addDecls(gamma1, xs, r.ts) 
      tcLet2(r.s, tc(gamma2, e))
    }    
    
    val letVars = l.bs map {_._1}
    val letExps = l.bs map {_._2}
    val tvs = letVars map {x => TypeVariable(x.name)}
    tcLet1(env, tvs, l.expr, tcl(env, letExps))
  }
  
  def tcLet2(s: Subst, r: Result) = Result(s compose r.s, r.t)
  
  def tcLetRec(env: TypeEnv, l: LetRecExpression): Result = {
    def newBVar(x: TypeVariable, tvn: TypeVariable) = (x, TypeScheme(Nil, tvn))
    def newBVars(xs: List[TypeVariable]) = xs map {x => newBVar(x, newTyvar)}
    
    def oldBVar(ts : TypeScheme) = ts.t
    
    def tcLetRec1(te: TypeEnv, nbvs: List[Pair[TypeVariable, TypeScheme]], e: Expression, r: ResultL) = {
      val gamma1 = te.sub(r.s)
      val nbvs1 = TypeEnv(nbvs).sub(r.s)
      val ts = nbvs1.al map {_._2.t}
      tcLetRec2(gamma1, nbvs1, e, mguL(r.ts zip ts ,r.s))
    }
    
    def tcLetRec2(gamma: TypeEnv, nbvs: TypeEnv, e: Expression, s: Subst) = {
      val nbvs1 = nbvs.sub(s)
      val ts = nbvs1.al map {_._2.t}
      val gamma1 = gamma.sub(s)
      val gamma2 = addDecls(gamma1, nbvs.al map {_._1}, ts)
      tcLet2(s, tc(gamma2, e))
    }
    
    val letRecVars = l.bs map {_._1}
    val letRecExps = l.bs map {_._2}
    val xs = letRecVars map {x => TypeVariable(x.name)}
    val nbvs = newBVars(xs)
    
    val env1 = TypeEnv(nbvs ::: env.al)
    val rl = tcl(env1, letRecExps)
    tcLetRec1(env, nbvs, l.expr, rl)
  }
  
  // type-checking of list of expressions
  def tcl(tes: TypeEnv, tss: List[Expression]) = {
    def tcl0(te: TypeEnv, ts: List[Expression]): ResultL = ts match {
      case Nil => ResultL(emptySubst, Nil) 
      case e :: es => tcl1(te, es, tc(te, e))
    }
  
    def tcl1(te: TypeEnv, es: List[Expression], r: Result) = {
      val gamma = te.sub(r.s)
      tcl2(r.s, r.t, tcl0(gamma, es))
    }
  
    def tcl2(phi: Subst, t: Type, r: ResultL) = ResultL(r.s compose phi, r.s(t) :: r.ts)
    
    tcl0(tes,tss)
  }
  
  def getFreeVars(t: Term): Set[Variable] = t match {
    case v: Variable => Set(v)
    case Constructor(_, args) => (Set[Variable]() /: args) {(vs, term) => vs ++ getFreeVars(term)}
    case LambdaAbstraction(x, term) => getFreeVars(term) - x
    case Application(head, arg) => getFreeVars(head) ++ getFreeVars(arg)
    case CaseExpression(sel, bs) => 
      getFreeVars(sel) ++ (Set[Variable]() /: bs) {(vs, b) => getFreeVars(b.term) -- b.pattern.args}
  }
  
  def tcProgram(): Unit = {
    val fs = (Map[String, Function]() /: p.fs) {(m, f) => m + (f.name -> f)}
    val vxs = (Map[String, Vertex]() /: p.fs) {(m, f) => m + (f.name -> Vertex(f.name))}
    var arcs = (List[Arc]() /: p.fs) {(a, f) => a ::: (getFreeVars(f.lam).toList map {t => Arc(vxs(f.name), vxs(t.name))})}
    val g = Graph(vxs.values.toList, arcs)
    val sccs = analizeDependencies(g)
    for (f <- p.fs) {
      var expr: Expression = Variable(f.name)
      for (scc <- sccs){
        val bs = scc.vs.toList map (x => (Variable(x.name), fs(x.name).lam))
        if (scc.recursive){
          expr = LetRecExpression(bs, expr)
        } else {
          expr = LetExpression(bs, expr)
        }
      }       
      f.`type` = tc(TypeEnv(Nil), expr).t
    }
  }
  
  def tcTerm(term: Term) = {
    val fs = (Map[String, Function]() /: p.fs) {(m, f) => m + (f.name -> f)}
    val vxs = (Map[String, Vertex]() /: p.fs) {(m, f) => m + (f.name -> Vertex(f.name))}
    var arcs = (List[Arc]() /: p.fs) {(a, f) => a ::: (getFreeVars(f.lam).toList map {t => Arc(vxs(f.name), vxs(t.name))})}
    val g = Graph(vxs.values.toList, arcs)
    val sccs = analizeDependencies(g)
    var expr: Expression = term
    for (scc <- sccs){
      val bs = scc.vs.toList map (x => (Variable(x.name), fs(x.name).lam))
      if (scc.recursive){
        expr = LetRecExpression(bs, expr)
      } else {
        expr = LetExpression(bs, expr)
      }
    }       
    tc(TypeEnv(Nil), expr).t
  }
  
}