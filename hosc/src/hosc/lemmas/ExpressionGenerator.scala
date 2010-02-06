package hosc.lemmas

import scala.collection.mutable.{ArrayBuffer, Buffer}
import hosc.HLanguage._
import hosc.DataConstructor
import hosc.TermAlgebra
import hosc.TypeInferrer
import hosc.LangUtils

/** 
 * Naive (slow and memory consuming) but simple
 * generator of expressions of a given size
 */
class ExpressionGenerator(val program: Program) {
	
  val globals: List[Variable] = 
    program.fs.map {d => 
      val gv = Variable(d.name)
      gv.global = true
      gv
    }
  
  val cons: List[DataConstructor] = program.ts flatMap {_.cons}
  
  val typeInferrer = new TypeInferrer(program.ts)
  
  def generateAll(maxSize: Int, vars: List[Variable]): Buffer[Expression] = {
    val buf = new ArrayBuffer[Expression]()
    for (i <- 1 to maxSize) {
      buf ++= generate(i, vars)
    }
    buf
  }
  
  def generate(size: Int, vars: List[Variable]): Buffer[Expression] = {
    val buf = new ArrayBuffer[Expression]()
    if (size == 0) {
      return buf
    }
    addExps(buf, generateApp(size, vars))
    addExps(buf, generateVars(size, vars))
    addExps(buf, generateCtrs(size, vars))
    addExps(buf, generateLams(size, vars))
    addExps(buf, generateCases(size, vars))
    buf
  }
  
  private def addExps(buf: Buffer[Expression], exps: Buffer[Expression]): Unit = {
    for (exp <- exps) {
      try {
        buf += typeCheck(exp)
      } catch {
        case e =>
      }
    }
  }

  private def generateVars(size: Int, vars: List[Variable]): Buffer[Expression] = {
    val res = new ArrayBuffer[Expression]()
    size match {
      case 1 => { 
        res ++= vars
        res ++= globals
      }
      case _ =>
	}
    res
  }
  
  private def generateApp(size: Int, vars: List[Variable]): Buffer[Expression] = {
    val res = new ArrayBuffer[Expression]()
    for (i <- 1 to (size - 1)) {
      val e1s = generate(i, vars)
      val e2s = generate(size - i, vars)
      for (e1 <- e1s; e2 <- e2s) {
        e1 match {
          case Constructor(_, _) =>
          case _ => res += Application(e1, e2)
        }
      }
    }
    res
  }
  
  private def generateCtrs(size: Int, vars: List[Variable]): Buffer[Expression] = {
    val res = new ArrayBuffer[Expression]()
    val maxArity = size - 1
    val ctrs = cons filter {_.args.size <= maxArity}
    for (ctr <- ctrs) {
      val arity = ctr.args.size
      val argsBuf = generateList(arity, size - 1, vars)
      for (args <- argsBuf) {
        res += Constructor(ctr.name, args) 
      } 
    }
    res
  }
  
  private def generateLams(size: Int, vars: List[Variable]): Buffer[Expression] = {
    val exps = new ArrayBuffer[Expression]()
    val arg = TermAlgebra.newVar
    val extVars = arg :: vars
    val bodies = generate(size - 1, extVars)
    for (body <- bodies) {
      exps += LambdaAbstraction(arg, body)
    }
    exps
  }
  
  private def generateCases(size: Int, vars: List[Variable]): Buffer[Expression] = {
    val res = new ArrayBuffer[Expression]()
    val maxConsN = size - 2
    val typeCons = program.ts filter {_.cons.size <= maxConsN}
    for (typeCon <- typeCons) {
      val consN = typeCon.cons.size
      for (i <- 1 to (size - consN - 1)) {
        val branchLists = generateBranches(typeCon.cons, size - 1 - i, vars)
        val selectors = generate(i, vars)
        for (selector <- selectors) {
          for (branchList <- branchLists) {
            res += CaseExpression(selector, branchList)
          }
        }
      }
    }
    res
  }
  
  private def generateBranches(dcons: List[DataConstructor], totalExpSize: Int, vars: List[Variable]): Buffer[List[Branch]] = {
    assert(totalExpSize >= dcons.size)
    val res = new ArrayBuffer[List[Branch]]()
    dcons match {
      case Nil => res += Nil
      case d :: ds => {
        val ptVars = d.args map {_ => TermAlgebra.newVar()}
        val pt = Pattern(d.name, ptVars)
        for (i <- 1 to (totalExpSize - ds.size)) {
          val otherBranchesS = generateBranches(ds, totalExpSize - i, vars)
          val bodies: Buffer[Expression] = generate(i, ptVars ::: vars)
          for (body <- bodies) {
            val branch = Branch(pt, body)
            for (otherBranches <- otherBranchesS) {
              res += branch :: otherBranches
            }
          }
        }
      }
    }
    res
  }
  
  def generateList(listSize: Int, totalExpSize: Int, vars: List[Variable]): Buffer[List[Expression]] = {
    assert(totalExpSize >= listSize)
    val res = new ArrayBuffer[List[Expression]]()
    if (listSize == 0) {
      res += Nil
    } else {
      for (i <- 1 to (totalExpSize - listSize + 1)) {
        val heads: Buffer[Expression] = generate(i, vars)
        val tails: Buffer[List[Expression]] = generateList(listSize - 1, totalExpSize - i, vars)
        for (head <- heads; tail <- tails) {
          res += (head :: tail)
        }
      }
    }
    res
  }
  
  private def typeCheck(exp: Expression): Expression = {
    val p1 = Program(program.ts, exp, program.fs)
    val e1 = LangUtils.hl0ToELC(p1)
    typeInferrer.inferType(e1)
    exp
  }
}
