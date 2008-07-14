package hosc;

import EnrichedLambdaCalculus._
import GraphAnalysis._
import HLanguage.{Application => Application0, Variable => Variable0, CaseExpression => CaseExpression0, 
  Branch => Branch0, Pattern => Pattern0,
  Constructor => Constructor0, LambdaAbstraction => LambdaAbstraction0, 
  LetExpression => LetExpression0, LetRecExpression => LetRecExpression0, Expression => Expression0, Program => Program0, _}
import HLanguage1._
import TermAlgebra._
import util.Formatter._

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
    case LetRecExpression0(bs, lexpr) => 
      LetRecExpression(bs map {b => (Variable(b._1.name), hl0ToELC(b._2))}, hl0ToELC(lexpr))
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
    val fs = (Map[String, Function]() /: p.fs) {(m, f) => m + (f.name -> f)}
    val vxs = (Map[String, Vertex]() /: p.fs) {(m, f) => m + (f.name -> Vertex(f.name))}
    var arcs = (List[Arc]() /: p.fs) {(a, f) => a ::: (getFreeVars(f.lam).toList map {t => Arc(vxs(f.name), vxs(t.name))})}
    val g = Graph(vxs.values.toList, arcs)
    val sccs = analyzeDependencies(g)
    var expr: Expression0 = p.goal
    for (scc <- sccs){
      val bs = scc.vs.toList map (x => (Variable0(x.name), fs(x.name).lam))
      if (scc.recursive){
        expr = LetRecExpression0(bs, expr)
      } else {
        expr = LetExpression0(bs, expr)
      }
    }
    println(format(expr))
    expr
  }
  
  private def getFreeVars(expr: Expression0): Set[Variable0] = expr match {
    case v: Variable0 => Set(v)
    case Constructor0(_, args) => (Set[Variable0]() /: args) {(vs, term) => vs ++ getFreeVars(term)}
    case LambdaAbstraction0(x, term) => getFreeVars(term) - x
    case Application0(head, arg) => getFreeVars(head) ++ getFreeVars(arg)
    case CaseExpression0(sel, bs) => 
      getFreeVars(sel) ++ (Set[Variable0]() /: bs) {(vs, b) => vs ++ (getFreeVars(b.term) -- b.pattern.args)}
    case LetRecExpression0(bs, expr) => 
      ((getFreeVars(expr) /: bs) {(vs, b) => vs ++ getFreeVars(b._2)}) -- (bs map {_._1})
    case LetExpression0(bs, expr) => 
      ((getFreeVars(expr) /: bs) {(vs, b) => vs ++ getFreeVars(b._2)}) -- (bs map {_._1})
  }
  
  def hl0ToELC(p: Program0): Expression = hl0ToELC(normalize(p))
}
