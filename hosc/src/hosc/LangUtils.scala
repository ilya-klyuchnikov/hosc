package hosc;

import EnrichedLambdaCalculus._
import GraphAnalysis._
import HLanguage.{Application => Application0, Variable => Variable0, CaseExpression => CaseExpression0, 
  Branch => Branch0, Pattern => Pattern0, Function=>Function0,
  Constructor => Constructor0, LambdaAbstraction => LambdaAbstraction0, 
  LetExpression => LetExpression0, LetRecExpression => LetRecExpression0, Expression => Expression0, Program => Program0, Choice => Choice0}

import scala.collection.mutable.ListBuffer

object LangUtils {
  private def hl0ToELC(expr: Expression0): Expression = expr match {
    case Variable0(n) => Variable(n)
    case Constructor0(n, args) => Constructor(n, args map hl0ToELC)
    case LambdaAbstraction0(v, e) => LambdaAbstraction(Variable(v.name), hl0ToELC(e))
    case Application0(h, a) => Application(hl0ToELC(h), hl0ToELC(a))
    case CaseExpression0(sel, bs) => 
      CaseExpression(hl0ToELC(sel), 
          bs map {b => Branch(Pattern(b.pattern.name, b.pattern.args map {v => Variable(v.name)}), hl0ToELC(b.term))})
    case Choice0(e1, e2) => Choice(hl0ToELC(e1), hl0ToELC(e2))
    case LetExpression0(bs, lexpr) => 
      LetExpression(bs map {b => (Variable(b._1.name), hl0ToELC(b._2))}, hl0ToELC(lexpr))
    case LetRecExpression0(b, lexpr) => 
      LetRecExpression((Variable(b._1.name), hl0ToELC(b._2)) :: Nil, hl0ToELC(lexpr))
  }
  
  private def normalize(p: Program0): Expression = {
    val fs = (Map[String, Function0]() /: p.fs) {(m, f) => m + (f.name -> f)}
    val vxs = (Map[String, Vertex]() /: p.fs) {(m, f) => m + (f.name -> Vertex(f.name))}
    var arcs = (List[Arc]() /: p.fs) {(a, f) => a ::: (getFreeVars(f.lam).toList map {t => Arc(vxs(f.name), vxs(t.name))})}
    val g = Graph(vxs.values.toList, arcs)
    val sccs = analyzeDependencies(g)
    var expr: Expression = hl0ToELC(p.goal)
    for (scc <- sccs){
      val bs = scc.vs.toList map (x => (Variable(x.name), hl0ToELC(fs(x.name).lam)))
      if (scc.recursive){
        expr = LetRecExpression(bs, expr)
      } else {
        expr = LetExpression(bs, expr)
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
    case Choice0(e1, e2) => getFreeVars(e1) ++ getFreeVars(e2)
    case LetRecExpression0(bs, expr) => getFreeVars(expr) ++ getFreeVars(bs._2) - bs._1
    case LetExpression0(bs, expr) => 
      ((getFreeVars(expr) /: bs) {(vs, b) => vs ++ getFreeVars(b._2)}) -- (bs map {_._1})
  }
  
  def hl0ToELC(p: Program0): Expression = {
    val r = normalize(p)
    r
  }
  def format(expr: Expression0): String = {
    val writer = new java.io.StringWriter()
    expr.toDoc.format(120, writer)
    writer.toString
  }  
  def format(expr: Expression): String = {
    val writer = new java.io.StringWriter()
    expr.toDoc.format(120, writer)
    writer.toString
  }
   
  def canonize(tt: Expression0):Expression0 = tt match {
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
      val sortedBranches = bs sort TermAlgebra.compareB
      val canonizedBranches = sortedBranches map {b => Branch0(b.pattern, canonize(b.term))}
      val canonizedSelector = canonize(sel)
      CaseExpression0(canonizedSelector, canonizedBranches)
    }
    case Choice0(e1, e2) => Choice0(canonize(e1), canonize(e2))
    case l: LetExpression0 => throw new IllegalArgumentException()
    case l: LetRecExpression0 => throw new IllegalArgumentException()
  }
  
  def canonize(p: Program0): Program0 = {
    Program0(p.ts, canonize(p.goal), p.fs map {f => Function0(f.name, canonize(f.lam).asInstanceOf[LambdaAbstraction0])})
  }
  
}
