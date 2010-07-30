package hosc

import HLanguage._
import ProcessTree._
import MSG._
import TermAlgebra._

/**
 * CodeConstructor extracts a residual program from a partial process tree,
 * which is produced by ANY HOSC transformer.
 * 
 * HOSC transformer is a program transformer satisfying HOSC transformation
 * relation. HOSC transformation relation is described in:
 * 
 * Ilya Klyuchnikov. Supercompiler HOSC: proof of correctness. 
 * Preprint 2010-31. Keldysh Institute of Applied Mathematics.
 */
class CodeConstructor(val originalProgram: Program, val tree: ProcessTree, freeVarsInLetrecs: Boolean) {

  def generateProgram() = Program(originalProgram.ts, fold(tree.rootNode), Nil)

  private val vNames = "xyzuvwprst".toArray
  private val fNames = "fgh".toArray
  private var fUsed = Set[String]() ++ (getAllVars(tree.rootNode.expr) map { v => v.name })

  private def construct(node: Node): Expression = node.expr match {

    case LetExpression(bs, t) => {
      val node0 :: nodes = node.children
      val bindValues = nodes map fold
      val subs = Map[Variable, Expression]() ++ ((bs map (_._1)) zip bindValues)
      fold(node0) / subs
    }

    case _ => decompose(node.expr) match {

      case ObservableVar(v) =>
        Variable(v.name)

      case ObservableCon(c) =>
        Constructor(c.name, node.children map fold)

      case ObservableVarApp(v, app) =>
        constructApplication(Variable(v.name), node.children map fold)

      case ObservableLam(l) =>
        LambdaAbstraction(Variable(l.v.name), fold(node.children.head))

      case Context(RedexLamApp(lam, app)) =>
        fold(node.children.head)

      // TODO: get rid off this extension (move to experimental branch)
      case Context(RedexChoice(choice)) => node.children match {
        case e1 :: e2 :: Nil => Choice(fold(e1), fold(e2))
        case _ => throw new IllegalStateException("exprected exactly 2 child nodes here...")
      }

      // TODO: get rid off this extension (move to experimental branch)
      case Context(RedexCaseCon(c, CaseExpression(sel, Nil))) => {
        CaseExpression(sel, Nil)
      }

      case Context(RedexCaseCon(c, ce)) =>
        fold(node.children.head)

      // TODO: can we extract logic of ticks into somewhere else?
      case Context(RedexCall(_)) =>
        fold(node.children.head).incrTicks()

      case Context(RedexCaseVar(_, CaseExpression(_, bs))) => {
        val newBs = (bs zip node.children.tail) map { case (Branch(p, _), n) => Branch(p, fold(n)) }
        CaseExpression(fold(node.children.head), newBs)
      }

    }
  }

  private def fold(node: Node): Expression =
    if (node.getRepParent != null) {
      val fNode = node.getRepParent()
      val (f, args) = fNode.signature
      val app = constructApplication(f, args)
      applySubstitution(app, Instance.findSubst(fNode.expr, node.expr))
    } else {
      lazy val traversed = construct(node)
      tree.leafs.filter(_.repeatedOf == node) match {
        case Nil => traversed
        case repeatNodes => {
          val (f, vars) = createSignature(node, repeatNodes)
          node.signature = (f, vars)
          val newVars = vars map { p => createVar() }
          val sub = Map(vars zip newVars: _*)
          val body = traversed / sub
          LetRecExpression((f, constructLambda(newVars, body)), constructApplication(f, vars))
        }
      }
    }

  private def createSignature(fNode: Node, recNodes: List[Node]) = {
    var vars: List[Variable] = TermAlgebra.getFreeVars(fNode.expr)
    if (freeVarsInLetrecs) {
      var changedVars = Set[Variable]()
      for (n <- recNodes) {
        val betaT = n.expr
        val sub = Instance.findSubst(fNode.expr, betaT)
        val args0 = sub map { p => p._1 }
        changedVars = changedVars ++ args0
      }
      vars = vars filter { changedVars.contains }
    }
    (Variable(createFName()), vars)
  }

  private var i = 0;
  private val treeVars = tree.rootNode.getAllVars()
  private def createVar(): Variable = {
    var nv: Variable = null
    do {
      nv = varFor(i)
      i += 1
    } while (treeVars contains nv)
    nv
  }

  private var fi = 0;
  private def createFName(): String = {
    var name: String = null
    do {
      name = fName(fi)
      fi += 1
    } while (fUsed contains name)
    fUsed = fUsed + name
    name
  }

  private def varFor(j: Int) = {
    if (j < 10) Variable("" + vNames(j))
    else Variable("" + vNames(j % 10) + Integer.toString(j / 10))
  }

  private def fName(j: Int): String = {
    if (j < 3) "" + fNames(j)
    else fNames(j % 3) + Integer.toString(j / 3)
  }

  private def isSynthetic(v: Variable) = v.name startsWith "$";

}