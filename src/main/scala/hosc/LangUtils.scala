package hosc;

import EnrichedLambdaCalculus._
import GraphAnalysis._
import HLanguage.{
  Application => Application0,
  Variable => Variable0,
  CaseExpression => CaseExpression0,
  Function => Function0,
  Constructor => Constructor0,
  LambdaAbstraction => LambdaAbstraction0,
  LetExpression => LetExpression0,
  LetRecExpression => LetRecExpression0,
  Expression => Expression0,
  Program => Program0
}

object LangUtils {
  def hl0ToELC(expr: Expression0): Expression = expr match {
    case Variable0(n) =>
      Variable(n)
    case Constructor0(n, args) =>
      Constructor(n, args map hl0ToELC)
    case LambdaAbstraction0(v, e) =>
      LambdaAbstraction(Variable(v.name), hl0ToELC(e))
    case Application0(h, a) =>
      Application(hl0ToELC(h), hl0ToELC(a))
    case CaseExpression0(sel, bs) =>
      CaseExpression(
          hl0ToELC(sel),
          bs.map(b => Branch(Pattern(b.pattern.name, b.pattern.args.map(v => Variable(v.name))), hl0ToELC(b.term)))
      )
    case LetExpression0(bs, lexpr) =>
      LetExpression(bs.map(b => (Variable(b._1.name), hl0ToELC(b._2))), hl0ToELC(lexpr))
    case LetRecExpression0(b, lexpr) =>
      LetRecExpression((Variable(b._1.name), hl0ToELC(b._2)) :: Nil, hl0ToELC(lexpr))
  }

  def normalize(p: Program0): Expression = {
    val fs = p.fs.foldLeft(Map[String, Function0]())((m, f) => m + (f.name -> f))
    val vxs =  p.fs.foldLeft(Map[String, Vertex]())((m, f) => m + (f.name -> Vertex(f.name)))
    val arcs = p.fs.foldLeft(List[Arc]())((a, f) => a ::: (getFreeVars(f.body).toList map {t => Arc(vxs(f.name), vxs(t.name))}))
    val g = Graph(vxs.values.toList, arcs)
    val sccs = analyzeDependencies(g)
    var expr: Expression = hl0ToELC(p.goal)
    for (scc <- sccs){
      val bs = scc.vs.toList.map(x => (Variable(x.name), hl0ToELC(fs(x.name).body)))
      if (scc.recursive)
        expr = LetRecExpression(bs, expr)
      else
        expr = LetExpression(bs, expr)
    }
    expr
  }

  private def getFreeVars(expr: Expression0): Set[Variable0] = expr match {
    case v: Variable0 =>
      Set(v)
    case Constructor0(_, args) =>
      args.foldLeft(Set[Variable0]())((vs, term) => vs ++ getFreeVars(term))
    case LambdaAbstraction0(x, term) =>
      getFreeVars(term) - x
    case Application0(head, arg) =>
      getFreeVars(head) ++ getFreeVars(arg)
    case CaseExpression0(sel, bs) =>
      getFreeVars(sel) ++  bs.foldLeft(Set[Variable0]())((vs, b) => vs ++ (getFreeVars(b.term) -- b.pattern.args))
    case LetRecExpression0(bs, expr) =>
      getFreeVars(expr) ++ getFreeVars(bs._2) - bs._1
    case LetExpression0(bs, expr) =>
      bs.foldLeft(getFreeVars(expr))((vs, b) => vs ++ getFreeVars(b._2)) -- bs.map(_._1)
  }
}
