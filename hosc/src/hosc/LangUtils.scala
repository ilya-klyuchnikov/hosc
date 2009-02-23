package hosc;

import EnrichedLambdaCalculus._
import GraphAnalysis._
import HLanguage.{Application => Application0, Variable => Variable0, CaseExpression => CaseExpression0, 
  Branch => Branch0, Pattern => Pattern0, Term => Term0, Function=>Function0,
  Constructor => Constructor0, LambdaAbstraction => LambdaAbstraction0, 
  LetExpression => LetExpression0, LetRecExpression => LetRecExpression0, Expression => Expression0, Program => Program0, _}
import HLanguage1._
import sc0.TermAlgebra0._
import sc1.TermAlgebra1._

import scala.collection.mutable.ListBuffer

object LangUtils {
  def hl0ToELC(expr: Expression0): Expression = expr match {
    case Variable0(n) => Variable(n)
    case Constructor0(n, args) => Constructor(n, args map hl0ToELC)
    case LambdaAbstraction0(v, e) => LambdaAbstraction(Variable(v.name), hl0ToELC(e))
    case Application0(h, a) => Application(hl0ToELC(h), hl0ToELC(a))
    case CaseExpression0(sel, bs) => 
      CaseExpression(hl0ToELC(sel), 
          bs map {b => Branch(Pattern(b.pattern.name, b.pattern.args map {v => Variable(v.name)}), hl0ToELC(b.term))})
    case LetExpression0(bs, lexpr) => 
      LetExpression(bs map {b => (Variable(b._1.name), hl0ToELC(b._2))}, hl0ToELC(lexpr))
    case LetRecExpression0(b, lexpr) => 
      LetRecExpression((Variable(b._1.name), hl0ToELC(b._2)) :: Nil, hl0ToELC(lexpr))
  }
  
  def hl1ToELC(expr: Expression1): Expression = expr match {
    case Variable1(n) => Variable(n)
    case Constructor1(n, args) => Constructor(n, args map hl1ToELC)
    case LambdaAbstraction1(v, e) => LambdaAbstraction(Variable(v.name), hl1ToELC(e))
    case Application1(h, a) => Application(hl1ToELC(h), hl1ToELC(a))
    case CaseExpression1(sel, bs) => 
      CaseExpression(hl1ToELC(sel), 
          bs map {b => Branch(Pattern(b.pattern.name, b.pattern.args map {v => Variable(v.name)}), hl1ToELC(b.term))})
    case LetExpression1(bs, lexpr) => 
      LetExpression(bs map {b => (Variable(b._1.name), hl1ToELC(b._2))}, hl1ToELC(lexpr))
    case LetRecExpression1(b, lexpr) => 
      LetRecExpression((Variable(b._1.name), hl1ToELC(b._2)) :: Nil, hl1ToELC(lexpr))
  }
  
  def normalize(p: Program0): Expression0 = {
    val fs = (Map[String, Function0]() /: p.fs) {(m, f) => m + (f.name -> f)}
    val vxs = (Map[String, Vertex]() /: p.fs) {(m, f) => m + (f.name -> Vertex(f.name))}
    var arcs = (List[Arc]() /: p.fs) {(a, f) => a ::: (getFreeVars(f.lam).toList map {t => Arc(vxs(f.name), vxs(t.name))})}
    val g = Graph(vxs.values.toList, arcs)
    val sccs = analyzeDependencies(g)
    var expr: Expression0 = p.goal
    for (scc <- sccs){
      val bs = scc.vs.toList map (x => (Variable0(x.name), fs(x.name).lam))
      if (scc.recursive){
        expr = bs.foldRight(expr){(b, e) => LetRecExpression0(b, e)}
      } else {
        expr = LetExpression0(bs, expr)
      }
    }
    //println(format(expr))
    expr
  }
  
  private def getFreeVars(expr: Expression0): Set[Variable0] = expr match {
    case v: Variable0 => Set(v)
    case Constructor0(_, args) => (Set[Variable0]() /: args) {(vs, term) => vs ++ getFreeVars(term)}
    case LambdaAbstraction0(x, term) => getFreeVars(term) - x
    case Application0(head, arg) => getFreeVars(head) ++ getFreeVars(arg)
    case CaseExpression0(sel, bs) => 
      getFreeVars(sel) ++ (Set[Variable0]() /: bs) {(vs, b) => vs ++ (getFreeVars(b.term) -- b.pattern.args)}
    case LetRecExpression0(bs, expr) => getFreeVars(expr) ++ getFreeVars(bs._2) - bs._1
    case LetExpression0(bs, expr) => 
      ((getFreeVars(expr) /: bs) {(vs, b) => vs ++ getFreeVars(b._2)}) -- (bs map {_._1})
  }
  
  def hl0ToELC(p: Program0): Expression = hl0ToELC(normalize(p))
  
  // converts hlanguage to hlanguage1
  def hlToHl1(term: Term0): Term1 = term match {
    case v @ Variable0(n) => val v1 = Variable1(n); v1.call = v.global; v1
    case Constructor0(n, args) => Constructor1(n, args map hlToHl1)
    case LambdaAbstraction0(v, e) => LambdaAbstraction1(Variable1(v.name), hlToHl1(e))
    case Application0(h, a) => Application1(hlToHl1(h), hlToHl1(a))
    case CaseExpression0(sel, bs) => 
      CaseExpression1(hlToHl1(sel), 
          bs map {b => Branch1(Pattern1(b.pattern.name, b.pattern.args map {v => Variable1(v.name)}), hlToHl1(b.term))})
  }  
  
  def hlToHl1(p: Pattern0): Pattern1 = {
    Pattern1(p.name, p.args map {v => Variable1(v.name)})
  }
  
  def hl1ToHl(e1: Term1, p: ListBuffer[Function0]): Term0 = e1 match {
    case Variable1(n) => Variable0(n)
    case Constructor1(n, args) => Constructor0(n, args map {e => hl1ToHl(e, p)})
    case LambdaAbstraction1(v, e) => LambdaAbstraction0(Variable0(v.name), hl1ToHl(e, p))
    case Application1(h, a) => Application0(hl1ToHl(h, p), hl1ToHl(a, p))
    case CaseExpression1(sel, bs) => 
      CaseExpression0(hl1ToHl(sel, p), 
          bs map {b => Branch0(Pattern0(b.pattern.name, b.pattern.args map {v => Variable0(v.name)}), hl1ToHl(b.term, p))})
    case lr: LetRecExpression1 => letrecToHl(lr, p)
  }
  
  def letrecToHl(letrec: LetRecExpression1, p: ListBuffer[Function0]): Term0 = { 
    Function0(letrec.binding._1.name, hl1ToHl(letrec.binding._2, p).asInstanceOf[LambdaAbstraction0]) +: p
    hl1ToHl(letrec.expr, p)
  }
  
  def hl1ToHl(p1: Program1): Program0 = {
    val functions = new ListBuffer[Function0]()
    val goal = hl1ToHl(p1.expr, functions)
    Program0(p1.ts, goal, functions.toList)
  }
  
  def format(term: Expression1): String = {
    val writer = new java.io.StringWriter()
    term.toDoc.format(120, writer)
    writer.toString
  }
  
  def format(expr: Expression0): String = {
    val writer = new java.io.StringWriter()
    expr.toDoc.format(120, writer)
    writer.toString
  }
   
  def canonize(tt: Term0):Term0 = tt match {
    case v: Variable0 => v
    case c@Constructor0(name, args) => {
      Constructor0(name, args map canonize)
    }
    case la@LambdaAbstraction0(x, term) => {
      LambdaAbstraction0(x, canonize(term))
    }
    case a@Application0(head, arg) => {
      Application0(canonize(head), canonize(arg))
    }
    case ce@CaseExpression0(sel, bs) => {
      val sortedBranches = bs sort compareB
      val canonizedBranches = sortedBranches map {b => Branch0(b.pattern, canonize(b.term))}
      val canonizedSelector = canonize(sel)
      CaseExpression0(canonizedSelector, canonizedBranches)
    }
  }
  
  def canonize(p: Program0): Program0 = {
    Program0(p.ts, canonize(p.goal), p.fs map {f => Function0(f.name, canonize(f.lam).asInstanceOf[LambdaAbstraction0])})
  }


  def canonize1(tt: Term1):Term1 = tt match {
    case v: Variable1 => v
    case c@Constructor1(name, args) => {
      val c1 = Constructor1(name, args map canonize1)
      c1.label = c.label
      c1
    }
    case la@LambdaAbstraction1(x, term) => {
      val la1 = LambdaAbstraction1(x, canonize1(term))
      la1.label = la.label
      la1
    }
    case a@Application1(head, arg) => {
      val a1 = Application1(canonize1(head), canonize1(arg))
      a1.label = a.label
      a1
    }
    case ce@CaseExpression1(sel, bs) => {
      val sortedBranches = bs sort compareB1
      val canonizedBranches = sortedBranches map {b => Branch1(b.pattern, canonize1(b.term))}
      val canonizedSelector = canonize1(sel)
      val ce1 = CaseExpression1(canonizedSelector, canonizedBranches)
      ce1.label = ce.label
      ce1
    }    
    case lr@LetRecExpression1((v, l), expr) => {
      val lr1 = LetRecExpression1((v, canonize1(l)), canonize1(expr))
      lr1.label = lr.label
      lr1
    }
  } 
  
}
