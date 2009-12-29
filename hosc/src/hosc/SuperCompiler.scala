package hosc

import HLanguage._
import HE._
import MSG._
import TermAlgebra._
import ProcessTree._
import LangUtils._

class SuperCompiler(program: Program){
  val emptyMap = Map[Variable, Expression]()
  val debug = false
  
  def driveExp(expr: Expression): List[Pair[Expression, Map[Variable, Expression]]] = expr match {
    case LetExpression(bs, t) => {
      (t, Map[Variable, Expression](bs map {p => (p._1, p._2)} :_*)) :: 
        (bs map {b => (b._2, emptyMap)}) 
    }
    case t => decompose(t) match {
      case ObservableVar(_) => Nil
      case ObservableCon(c) => c.args map {a => (a, emptyMap)}
      case ObservableVarApp(_, app) => extractAppArgs(app) map {a => (a, emptyMap)}
      case ObservableLam(l) => (l.t, emptyMap) :: Nil
      case context: Context => context.redex match {
        case RedexCall(v) => {
          val lam = program.getFunction(v.name).get.lam
          (context.replaceHole(freshBinders(lam)), emptyMap) :: Nil 
        }
        case RedexLamApp(lam, app) => 
          (context.replaceHole(applySubstitution(lam.t, Map(lam.v -> app.arg))), emptyMap) :: Nil
        case RedexCaseCon(c, ce) => {
          val b = ce.branches.find(_.pattern.name == c.name).get
          val sub = Map[Variable, Expression]() ++ (b.pattern.args zip c.args)
          (context.replaceHole(applySubstitution(b.term, sub)), emptyMap) :: Nil          
        }
        case RedexCaseVar(_, CaseExpression(sel, bs)) =>
          (sel, emptyMap) :: (bs map 
            {b => (replaceTerm(context.replaceHole(b.term), sel, Constructor(b.pattern.name, b.pattern.args)), emptyMap)})
      }
    }
  }  
  
  def buildProcessTree(e: Expression): ProcessTree = {
    val p = ProcessTree(e)
    if (debug) {
      println(program.toDocString)
    }
    while (!p.isClosed) {
      if (debug) { 
        println(p)
        println("==========")
      }
      val beta = p.leafs.find(!_.isProcessed).get
      val bExpr = beta.expr
      beta.expr match {
        case LetExpression(_, _) => drive(p, beta)
        case bTerm if canBeEnhanced_?(bTerm) => {
          beta.ancestors find equivalenceTest(bTerm) match {
            case Some(alpha) => beta.repeatedOf = alpha; 
            case None => {
              beta.ancestors find instanceTest(bTerm) match {
                case Some(alpha1) => makeAbstraction(p, beta, alpha1) 
                case None => { 
                  beta.ancestors find heByCouplingTest(bTerm) match {
                    case Some(alpha) => {
                      if (debug) {
                        println("GENERALIZATION FROM SC0")
                      }
                      makeAbstraction(p, alpha, beta)
                    }
                    case None => drive(p, beta)
                  }
                }
              }
            }
          }
        }
        case _ => drive(p, beta)
      }      
    }
    renameVars(p)
  }
  
  private def instanceTest(bTerm: Expression)(aNode: Node): Boolean = aNode.expr match {
    case LetExpression(_, _) => false
    case aTerm => sameRedex(aTerm, bTerm) && instanceOf(aTerm, bTerm);
  }
  
  private def equivalenceTest(bTerm: Expression)(aNode: Node): Boolean = aNode.expr match {
    case LetExpression(_, _) => false
    case aTerm => equivalent(aTerm, bTerm);
  }
  
  private def heByCouplingTest(bTerm: Expression)(aNode: Node): Boolean = aNode.expr match {
    case LetExpression(_, _) => false
    case aTerm => sameRedex(aTerm, bTerm) && heByCoupling(aTerm, bTerm);
  }
  
  def canBeEnhanced_?(t: Expression) = decompose(t) match {
    case c@ContextHole(_) => c.redex match { 
      case r: RedexCall => true
      case r: RedexCaseVar => true
      case _ => false
    }
    case c@ContextApp(_, _) => c.redex match { 
      case r: RedexCall => true
      case r: RedexCaseVar => true
      case _ => false
    }
    case c@ContextCase(_, _) => c.redex match { 
      case r: RedexCall => true
      case r: RedexCaseVar => true
      case _ => false
    }
    case _ => false
  }
  
  def canBeEnhanced1_?(t: Expression) = decompose(t) match {
    case c@ContextCase(_, _) => c.redex match { 
      case r: RedexCall => true
      case r: RedexCaseVar => true
      case _ => false
    }
    case _ => false
  }
  
  def sameRedex(t1: Expression, t2: Expression) : Boolean = (decompose(t1), decompose(t2)) match {
    case (c1: Context, c2: Context) => true//c1.redex.getClass() == c2.redex.getClass()
    case _ => false
  }
  
  def drive(t: ProcessTree, n: Node): Unit = {
    t.addChildren(n, driveExp(n.expr))
  }
  
  def makeAbstraction(t: ProcessTree, alpha: Node, beta: Node): Unit = {
    val aTerm = alpha.expr
    val bTerm = beta.expr
    val g = msg(aTerm, bTerm)
    if (g.sub1.isEmpty){
      t.replace(alpha, g.term)
    } else {
      if (debug){
        println(format(canonize(aTerm)))
        println(format(canonize(bTerm)))
      }
      var term = g.term
      var subs = g.sub1
      if (debug) {
        println(format((LetExpression(g.sub1, g.term))))
      }
      t.replace(alpha, LetExpression(g.sub1, g.term))
    }    
  }
  
  def renameVars(p: ProcessTree): ProcessTree = {
    val vars = p.rootNode.getAllVars()
    var i = 0
    def createVar(): Variable = {      
      var nv: Variable = null
      do {
        nv = varFor(i)
        i += 1
      } while (vars contains nv)
      nv
    }
    var map = Map[Variable, Variable]()
    for (v <- vars.toList) {
      if (isSynthetic(v)) {
        map = map + (v -> createVar)
      }
    }
    p.rootNode sub map
    p
  }
  
  private val vNames = "xyzuvwprst".toArray
  
  private def varFor(j: Int) = {
    if (j <= 9) 
      Variable("" + vNames(j))
    else 
      Variable("" + vNames(j % 10) + Integer.toString(j / 10))   
  }
  
  private def isSynthetic(v: Variable) = v.name startsWith "$" 
  
}