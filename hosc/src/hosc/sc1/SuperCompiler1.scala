package hosc.sc1

import HLanguage1._
import ProcessTree1._
import TermAlgebra1._
import MSG1._
import HE1._
import LangUtils._

class SuperCompiler1(val program: Program1, varsUtil: VarGen1) {
  var driver = new Driver1(varsUtil)
  
  def superCompile(): (ProcessTree1, Program1) = {
    val p = ProcessTree1(program.expr)
    while (!p.isClosed) {
      val beta = p.leafs.find(!_.isProcessed).get
      val bExpr = beta.expr
      beta.expr match {
        case bTerm: Term1 if canBeEnhanced_?(bTerm) => {
          beta.ancestors find equivalenceTest(bTerm) match {
            case Some(alpha) => beta.repeatedOf = alpha; 
            case None => {
              beta.ancestors find instanceTest(bTerm) match {
                case Some(alpha1) => {
                  println("INSTANCE!!")
                  makeAbstraction(p, beta, alpha1)
                } 
                case None => { 
                  beta.ancestors find heByCouplingTest(bTerm) match {
                    case Some(alpha) => {
                      println("GENERALIZING FROM SCP1")
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
    val codeConstructor = new CodeConstructor1(program, p, varsUtil)
    //renameVars(p)
    (p, codeConstructor.constructProgram(p.rootNode))
  }
  
  private def drive(t: ProcessTree1, n: Node1): Unit = {
    t.addChildren(n, driver.drive(n.expr))
  }
  
  def makeAbstraction(tree: ProcessTree1, alpha: Node1, beta: Node1): Unit = {
    val alphaTerm = alpha.expr.asInstanceOf[Term1]
    val betaTerm = beta.expr.asInstanceOf[Term1]
    val g = msg(alphaTerm, betaTerm)
    var generalizedTerm = g.term
    var subs = g.sub1
    //println(form(can(alphaTerm)))
    //println()
    //println(form(can(betaTerm)))
    var resSub = List[Substitution]()
    var set = Set[Variable1]()
    // eliminate letrecs mapping
    for (sub <- subs) {
      sub match {
        case (v1, v2: Variable1) if v1.call => generalizedTerm = generalizedTerm\\Map(v1 -> v2);
        case _ => resSub = sub :: resSub
      }
    }
    
    val let = LetExpression1(resSub, generalizedTerm) 
    //println(g)
    //println(form(let))
    //println("=======================")
    tree.replace(alpha, let)
  }
  
  private def instanceTest(bTerm: Term1)(aNode: Node1): Boolean = aNode.expr match {
    case aTerm: Term1 => /*sameRedex(aTerm, bTerm) &&*/  instanceOf(aTerm, bTerm);
    case _ => false
  }
  
  private def equivalenceTest(bTerm: Term1)(aNode: Node1): Boolean = aNode.expr match {
    case aTerm: Term1 => equivalent(aTerm, bTerm);
    case _ => false
  }

  private def heByCouplingTest(bTerm: Term1)(aNode: Node1): Boolean = aNode.expr match {
    case aTerm: Term1 => /*sameRedex(aTerm, bTerm) &&*/ heByCoupling(aTerm, bTerm);
    case _ => false
  }
  
  private def canBeEnhanced_?(t: Term1) = decompose1(t) match {
    case c: Context1 => c.redex match { 
      //case _: RedexCall1 => true
      //case _: RedexCaseVar1 => true
      //case _: RedexCaseVarApp1 => true
      case _: RedexLetRec1 => true
      case _ => false
    }
    case _: Observable1 => false
  }
  
  private def sameRedex(t1: Term1, t2: Term1) : Boolean = (decompose1(t1), decompose1(t2)) match {
    case (c1: Context1, c2: Context1) => c1.redex.getClass() == c2.redex.getClass()
    case _ => false
  }
  def renameVars(p: ProcessTree1): ProcessTree1 = {
    val vars = p.rootNode.getAllVars1()
    var i = 0
    var j = 0
    def createVar(v: Variable1): Variable1 = {
      if (v.call) {
        var nv: Variable1 = null
        do {
          nv = fvarFor(j)
          j += 1
        } while (vars contains nv)
        nv.call = true
        nv
      } else {
        var nv: Variable1 = null
        do {
          nv = varFor(i)
          i += 1
        } while (vars contains nv)
        nv
      }
    }
    var map = Map[Variable1, Variable1]()
    for (v <- vars.toList) {
      if (isSynthetic(v)) {
        map = map + (v -> createVar(v))
      }
    }
    p.rootNode sub map
    p
  }
  
  private val vNames = Array("xx", "yy", "zz", "uu", "vv", "ww", "pp", "rr", "ss", "tt");
  private val fNames = Array("ff", "gg", "hh");
  
  private def varFor(j: Int) = {
    if (j <= 9) 
      Variable1("" + vNames(j))
    else 
      Variable1("" + vNames(j % 10) + Integer.toString(j / 10))   
  }
  
  private def fvarFor(j: Int): Variable1 = {
    if (j < 3) 
      Variable1("" + fNames(j))
    else 
      Variable1(fNames(j % 3) + Integer.toString(j / 3))   
  }
  
  private def isSynthetic(v: Variable1) = v.name startsWith "$" 

}
