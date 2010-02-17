package hosc.lemmas

import hosc.HLanguage._
import hosc.TermAlgebra

case class GeneralizedExpression(exp: Expression, subst: Map[Variable, Expression]) {
  override def toString = "[" + exp + ",\n" + subst.mkString("("," "," )") + "]"
}

/*
 * one assumption: it doesn't extract variables for now
 */
object AdvancedExpressionGenerator {
	def generate(exp: Expression) = subGenerate(exp, Set(), Map())
 
	private def subGenerate(expr: Expression, boundVars: Set[Variable], subst: Map[Variable, Expression]): List[GeneralizedExpression] = 
    expr match {
	  case v: Variable => {
	    if (boundVars(v)) {
	      List(GeneralizedExpression(v, subst))
	    } else {
	      //val nv = TermAlgebra.newVar
	      //List(GeneralizedExpression(v, subst), GeneralizedExpression(nv, subst + (nv -> v)))
          List(GeneralizedExpression(v, subst))
	    }
     }
      case c@Constructor(n, args) => {
        val fv = TermAlgebra.getFreeVars(c)
        val me = if (fv.forall{!boundVars(_)}) {
          val nv = TermAlgebra.newVar
          GeneralizedExpression(nv, subst + (nv -> c))
        } else {
          null
        }
        val start: List[List[GeneralizedExpression]] = List(Nil)
        val argsGen = args.map{subGenerate(_, boundVars, subst)}
        val argsLists = argsGen.foldRight(start){(xxx, res) => List.flatten(xxx map {a => res map {a :: _}})}
        val res1 = argsLists map { args =>
          val resSubst = args.foldRight(subst){(genExpr, s) => s ++ genExpr.subst}
          val consArgs = args.map{_.exp}
          GeneralizedExpression(Constructor(n, consArgs), resSubst)
        }
        if (me != null) {
          me :: res1
        } else {
          res1
        }
      }
      case ce@CaseExpression(sel, bs) => {
        val fv = TermAlgebra.getFreeVars(ce)
        val me = if (fv.forall{!boundVars(_)}) {
          val nv = TermAlgebra.newVar
          GeneralizedExpression(nv, subst + (nv -> ce))
        } else {
          null
        }
        val start: List[List[GeneralizedExpression]] = List(Nil)
        val selGs = subGenerate(sel, boundVars, subst)
        val branchBodies = bs map {b => subGenerate(b.term, boundVars ++ b.pattern.args, subst)}
        val branchLists = branchBodies.foldRight(start){(args, res) => (args map {arg => res map {arg :: _}}).flatten}
        val res1:List[List[GeneralizedExpression]] = branchLists map { branchList =>
          val mSubst = branchList.foldRight(subst){(genExpr, s) => s ++ genExpr.subst}
          val branches = (bs zip branchList) map {case (b, ge) => Branch(b.pattern, ge.exp)}
          selGs map {selG => GeneralizedExpression(CaseExpression(selG.exp, branches), mSubst ++ selG.subst)}
        }
        val res2 = List.flatten(res1)
        if (me != null) {
          me :: res2
        } else {
          res2
        }
      }
      case a@Application(a1, a2) => {
        val fv = TermAlgebra.getFreeVars(a)
        val me = if (fv.forall{!boundVars(_)}) {
          val nv = TermAlgebra.newVar
          GeneralizedExpression(nv, subst + (nv -> a))
        } else {
          null
        }
        val g1s = subGenerate(a1, boundVars, subst)
        val g2s = subGenerate(a2, boundVars, subst)
        val res1 = for(g1 <- g1s; g2 <- g2s) yield GeneralizedExpression(Application(g1.exp, g2.exp), subst ++ g1.subst ++ g2.subst)
        if (me != null) {
          me :: res1
        } else {
          res1
        }
      }
	}
}