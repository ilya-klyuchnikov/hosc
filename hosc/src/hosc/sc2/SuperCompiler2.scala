package hosc.sc2

import HLanguage1._
import LangUtils._
import sc1.ProcessTree1
import sc1.ProcessTree1._
import sc1.TermAlgebra1._
import sc1.MSG1._
import sc1.HE1._
import sc1.SuperCompiler1
import sc1.SuperCompiler1S
import sc1.CodeConstructor1
import sc1.VarGen1

class SuperCompiler2(val program: Program1, varsUtil: VarGen1) {
  
  val driver = new sc1.Driver1(varsUtil)
  def superCompile(): (ProcessTree1, Program1) = {
    var i = 0
    val p = ProcessTree1(program.expr)
    while (!p.isClosed) {
      i += 1
      println("#########################" + i)
      println(p)
      println("===========================")
      val beta = p.leafs.find(!_.isProcessed).get
      val bExpr = beta.expr
      if (!untransformedLetrec_?(beta)) {
      beta.expr match {
        case bTerm: Term1 if canBeEnhanced_?(bTerm) => {
          beta.ancestors find equivalenceTest(bTerm) match {
            case Some(alpha) => beta.repeatedOf = alpha; 
            case None => {
              beta.ancestors find instanceTest(bTerm) match {
                case Some(alpha1) => makeAbstraction(p, beta, alpha1) 
                case None => { 
                  beta.ancestors find heByCouplingTest(bTerm) match {
                    case Some(alpha) => makeAbstraction(p, alpha, beta)
                    case None => drive(p, beta)
                  }
                }
              }
            }
          }
        }
        case _ => drive(p, beta)
      }} else {drive(p, beta)}
    }
    val codeConstructor = new CodeConstructor1(program, p, varsUtil)
    (p, codeConstructor.constructProgram(p.rootNode))
  }
  
  private def drive(tree: ProcessTree1, node: Node1): Unit = 
    /*if (node.in == null) {
      tree.addChildren(node, driver.drive(node.expr))
    } else*/ node.expr match {
      case t: Term1 => if (!node.supercompiled && canBeEnhanced2_?(t) && !letrecDirectChild(node))  {
        //val sc1 = new SuperCompiler1S(Program1(program.ts, t), varsUtil)
        println("++++++++++++++++");
        val sc1 = new SuperCompiler1(Program1(program.ts, t), varsUtil)
        val (tree1, superCompiledProgram) = sc1.superCompile()
        //val superCompiledProgram = sc1.superCompile()
        node.expr = superCompiledProgram.expr
        node.supercompiled = true
        println("++++++++++++++")
        println("BEFORE SC: ")
        println(format(t))
        println("AFTER SC: ")
        println(format(superCompiledProgram.expr))
        println("++++++++++++++")          
      } else {
        tree.addChildren(node, driver.drive(node.expr))
      }
      case _ => tree.addChildren(node, driver.drive(node.expr))
    }
  
  
  private def untransformedLetrec_?(node: Node1): Boolean = node.supercompiled match {
    case false => node.expr match {
      case t: Term1 => decompose1(t) match {
        case context: Context1 => context.redex match {
          case RedexLetRec1(letrec) => {
            true
          }
          case _ => false
        }
        case _ => false
      }
      case _ => false
    }
    case true => false
  }
  
  private def letrecDirectChild(n: Node1): Boolean = {
    for (a <- n.ancestors) {
      if (a.children.size > 1) {
        return false
      } else {
        a.expr match {
          case possibleLetrec: Term1 => decompose1(possibleLetrec) match {
            case possibleLetRecContext: Context1 => possibleLetRecContext.redex match {
              case RedexLetRec1(_) => return true 
              case _ => 
            }
          case _ =>
          }
         case _ => 
        }
      }
    }
    false
  }
  
  def makeAbstraction(tree: ProcessTree1, alpha: Node1, beta: Node1): Unit = {
    val alphaTerm = alpha.expr.asInstanceOf[Term1]
    val betaTerm = beta.expr.asInstanceOf[Term1]
    val g = msg(alphaTerm, betaTerm)
    var t = g.term
    var subs = g.sub1
    println("GENERALIZING FROM SCP2")
    println(format(canonize1(alphaTerm)))
    println()
    println(format(canonize1(betaTerm)))
    
    var resSub = List[Substitution]()
    var set = Set[Variable1]()
    for (sub <- subs) {
      sub match {
        case (v1, v2: Variable1) if v1.call => t = t\\Map(v1 -> v2);
        case _ => resSub = sub :: resSub
      }
    }
    
    val let = LetExpression1(resSub, t) 
    println(g)
    println(format(let))
    println("=======================")
    tree.replace(alpha, let)
  }
  
  private def instanceTest(bTerm: Term1)(aNode: Node1): Boolean = aNode.expr match {
    case aTerm: Term1 => /*sameRedex(aTerm, bTerm) &&*/ instanceOf(aTerm, bTerm);
    case _ => false
  }
  
  private def equivalenceTest(bTerm: Term1)(aNode: Node1): Boolean = aNode.expr match {
    case aTerm: Term1 => {
      if (equivalent(aTerm, bTerm)) {
        return true;
      } else {
        return false;
      }
    }
    case _ => false
  }

  private def heByCouplingTest(bTerm: Term1)(aNode: Node1): Boolean = aNode.expr match {
    case aTerm: Term1 => {
      /*sameRedex(aTerm, bTerm) &&*/ 
        val he1 = heByCoupling(aTerm, bTerm)
        val he2 = HE2.heByCoupling(aTerm, bTerm)
        if (he1 == false && he2 == true) {
          println("HE2!!!!!!!!!!!")
          println(format(canonize1(aTerm)))
          println("***************")
          println(format(canonize1(bTerm)))
          println("***************")
        }
      
        he1
    }
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
  
  private def canBeEnhanced2_?(t: Term1) = decompose1(t) match {
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
}
