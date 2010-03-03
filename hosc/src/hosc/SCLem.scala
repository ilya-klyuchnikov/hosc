package hosc

import HLanguage._

class SCLem(program: Program, lemmas: List[Lemma]) extends SuperCompiler0(program){
  override def driveExp(expr: Expression): Option[List[Expression]] = {
    super.driveExp(expr) map {_ map {e =>
      var currentExpr = e
      var beforeLemma = e
      do {
        beforeLemma = currentExpr
        for (lemma <- lemmas) {
          currentExpr = applyLemma(currentExpr, lemma)
        }
      } while (currentExpr != beforeLemma)
      currentExpr
    }}
  }
  
  def applyLemma(exp: Expression, lemma: Lemma): Expression = if (Instance.instanceOf(lemma.from, exp)) {
    val sub = Instance.findSubst(lemma.from, exp)
    val e1 = lemma.to/sub
    e1
  } else exp match {
    case v: Variable => v
    case Constructor(n, args) => Constructor(n, args map {a => applyLemma(a, lemma)})
    case Application(e1, e2) => Application(applyLemma(e1, lemma), applyLemma(e2, lemma))
    case LambdaAbstraction(v, t) => LambdaAbstraction(v, applyLemma(t, lemma))
    case CaseExpression(sel, bs) => 
      CaseExpression(applyLemma(sel, lemma), bs map {b => Branch(b.pattern, applyLemma(b.term, lemma))})
    case Choice(e1, e2) => Choice(applyLemma(e1, lemma), applyLemma(e2, lemma))
    case let: LetExpression => throw new IllegalArgumentException("unexpected expr: " + let)
    case letrec: LetRecExpression => throw new IllegalArgumentException("unexpected expr: " + letrec)
  }
}