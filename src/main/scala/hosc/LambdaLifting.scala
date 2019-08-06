package hosc

import scala.collection.mutable.ListBuffer

import HLanguage._
import TermAlgebra._

object LambdaLifting {

  var i = 0
  def newVar() = {
    i += 1
    Variable("_" + i)
  }

  def lift(p: Program): Program = {
    val liftedGoal = lift(p.goal)
    val functions = new ListBuffer[Function]()

    val extractedGoal = extractGlobals(liftedGoal, functions)
    Program(p.ts, extractedGoal, functions.toList ::: p.fs)
  }

  // eliminates free variables from letrecs' bodies
  private def lift (e: Expression) : Expression = {
    var letrecOpt = findLetRec(e)
    var result = e
    while (letrecOpt.isDefined) {
      val LetRecExpression((f, e0), _) = letrecOpt.get
      val freeVars = TermAlgebra.getFreeVars(e0)
      for (v <- freeVars.toList) {
        result = injectArgument(result, f, v)
      }
      letrecOpt = findLetRec(result)
    }
    result
  }

  // traverses expression top-down and finds the first letrec with free variable in its body
  def findLetRec(e: Expression) : Option[LetRecExpression] = e match {
    case Variable(_) => None
    case Constructor(_, args) => args.foldLeft(None: Option[LetRecExpression]){(r, expr) => r.orElse(findLetRec(expr))}
    case LambdaAbstraction(_, expr) => findLetRec(expr)
    case Application(head, arg) => findLetRec(head).orElse(findLetRec(arg))
    case CaseExpression(sel, branches) => branches.foldLeft(findLetRec(sel)){(r, branch) => r.orElse(findLetRec(branch.term))}
    case letrec@LetRecExpression((f, e1), e2) => {
      val e1FreeVars = TermAlgebra.getFreeVars(e1).filterNot(_ == f)
      if (e1FreeVars.isEmpty)
        findLetRec(e1).orElse(findLetRec(e2))
      else
        Some(letrec)
    }
    case l:LetExpression => throw new IllegalArgumentException("Unexpected let: " + l)
  }

  private def injectArgument(e: Expression, f: Variable, v0: Variable): Expression = e match {
    case v@Variable(_) =>
      if (v==f) Application(f, v0) else v
    case Constructor(name, args) =>
      Constructor(name, args map {injectArgument(_, f, v0)})
    case LambdaAbstraction(v, body) =>
      LambdaAbstraction(v, injectArgument(body, f, v0))
    case Application(head, arg) =>
      Application(injectArgument(head, f, v0), injectArgument(arg, f, v0))
    case CaseExpression(sel, branches) =>
      CaseExpression(injectArgument(sel, f, v0), branches map {b => Branch(b.pattern, injectArgument(b.term, f, v0))})
    case LetRecExpression((f1, e1), e2) =>
      if (f1==f) {
        val nv = newVar()
        LetRecExpression((f1, LambdaAbstraction(nv, injectArgument(e1, f, v0)\\Map(v0 -> nv))), injectArgument(e2, f, v0))
      } else {
        LetRecExpression((f1, injectArgument(e1, f, v0)), injectArgument(e2, f, v0))
      }
    case l:LetExpression => throw new IllegalArgumentException("Unexpected let: " + l)
  }

  // TODO:
  // 1. to use buffer is not pure functional way
  // 2. ideally a type of this function should be something like:
  //   extractG[T <:Expression](e1: T): T
  // that is the type of the result = the type of the first argument
  private def extractGlobals(e1: Expression, p: ListBuffer[Function]): Expression = e1 match {
    case v@Variable(_) => v
    case Constructor(n, args) => Constructor(n, args map {e => extractGlobals(e, p)})
    case LambdaAbstraction(v, e) => LambdaAbstraction(v, extractGlobals(e, p))
    case Application(h, a) => Application(extractGlobals(h, p), extractGlobals(a, p))
    case CaseExpression(sel, bs) =>
      CaseExpression(extractGlobals(sel, p),
          bs map {b => Branch(b.pattern, extractGlobals(b.term, p))})
    case lr: LetRecExpression => letrecToHl(lr, p)
    case l:LetExpression => throw new IllegalArgumentException("Unexpected let: " + l)
  }

  private def letrecToHl(letrec: LetRecExpression, p: ListBuffer[Function]): Expression = {
    extractGlobals(letrec.binding._2, p) match {
      case x =>  {
        Function(letrec.binding._1.name, x) +: p
        extractGlobals(letrec.expr, p)
      }
    }
  }

}
