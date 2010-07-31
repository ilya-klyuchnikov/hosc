package hosc

import HLanguage._
import TermAlgebra._

object MSG {

  type Substitution = Tuple2[Variable, Expression]
  type DoubleSubstitution = Tuple3[Variable, Expression, Expression]
  case class Generalization(term: Expression, sub1: List[Substitution], sub2: List[Substitution])
  case class Generalization2(term: Expression, dSub: List[DoubleSubstitution])

  def msg(term1: Expression, term2: Expression): Generalization = {
    val Generalization2(gTerm, dSub) = simplify(generalizeCoupled(term1, term2))
    val s1 = dSub.map { case (v, e1, e2) => (v, e1) }
    val s2 = dSub.map { case (v, e1, e2) => (v, e2) }
    Generalization(gTerm, s1, s2)
  }

  private def generalizeCoupled(e1: Expression, e2: Expression): Generalization2 =
    if (!HE.heByCoupling(e1, e2)) {

      val v = newVar()
      Generalization2(v, List((v, e1, e2)))

    } else {

      (e1, e2) match {

        case (Variable(n1), Variable(n2)) if n1 == n2 =>
          Generalization2(e1, List())

        case (Constructor(n1, args1), Constructor(n2, args2)) if n1 == n2 => {
          val subR = (args1, args2).zipped.map { generalizeCoupled }
          val genArgs = subR map { _.term }
          val sub = subR.foldRight(List[DoubleSubstitution]()) { _.dSub ++ _ }
          Generalization2(Constructor(n1, genArgs), sub)
        }

        case (LambdaAbstraction(v1, body1), LambdaAbstraction(v2, body2)) => {
          val freshVar = newVar

          val freshBody1 = applySubstitution(body1, Map(v1 -> freshVar))
          val freshBody2 = applySubstitution(body2, Map(v2 -> freshVar))
          val genBody = generalizeCoupled(freshBody1, freshBody2)

          Generalization2(LambdaAbstraction(freshVar, genBody.term), genBody.dSub)
        }

        case (Application(h1, arg1), Application(h2, arg2)) => {
          val Generalization2(genHead, sub1) = generalizeCoupled(h1, h2)
          val Generalization2(genArg, sub2) = generalizeCoupled(arg1, arg2)
          Generalization2(Application(genHead, genArg), sub1 ++ sub2)
        }

        case (CaseExpression(sel1, bs1), CaseExpression(sel2, bs2)) => {
          val Generalization2(genSel, sub) = generalizeCoupled(sel1, sel2)

          // TODO: get rid of it later
          val bs1s = bs1 sort compareB
          val bs2s = bs2 sort compareB

          // fresh branches
          val freshBs: List[(Pattern, Expression, Expression)] =
            (bs1s, bs2s).zipped.map { (b1, b2) =>
              val freshPatVars = b1.pattern.args
              val freshPat = Pattern(b1.pattern.name, freshPatVars)

              val freshB1 = applySubstitution(b1.term, Map(b1.pattern.args zip freshPatVars: _*))
              val freshB2 = applySubstitution(b2.term, Map(b2.pattern.args zip freshPatVars: _*))

              (freshPat, freshB1, freshB2)
            }

          // generalized branches
          val genBs: List[(Pattern, Generalization2)] =
            freshBs map { case (p, t1, t2) => (p, generalizeCoupled(t1, t2)) }

          val genCase = CaseExpression(genSel, genBs map { case (p, Generalization2(t, _)) => Branch(p, t) })
          val genSub = genBs.map { _._2 }.foldRight(sub) { _.dSub ++ _ }

          Generalization2(genCase, genSub)
        }

        case (t1, t2) => {
          val nv = newVar
          Generalization2(nv, List((nv, t1, t2)))
        }

      }

    }

  private def simplify(gen2: Generalization2): Generalization2 = {
    gen2.dSub match {
      case Nil => gen2
      case (el@(v, e1, e2)) :: els => {
        val Generalization2(simpledTerm, simpledSub) = simplify(Generalization2(gen2.term, els))
        val (same, other) = simpledSub partition { case (_, t1, t2) => e1 == t1 && e2 == t2 }
        val sub = Map(same map { case (v1, _, _) => (v1, v) }: _*)
        val term = applySubstitution(simpledTerm, sub)
        Generalization2(term, el :: other)
      }
    }
  }

}
