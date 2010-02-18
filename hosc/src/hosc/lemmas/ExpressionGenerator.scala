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
  
  def generate(size: Int, vars: List[Variable]): Buffer[Expression] = {
    timeOfChecker = 0
    timeInBuf = 0
    val start = System.currentTimeMillis
    val buf = new ArrayBuffer[Expression]()
    if (size == 0) {
      return buf
    }
    addExps(buf, generateApp(size, vars))
    addExps(buf, generateVars(size, vars))
    addExps(buf, generateCtrs(size, vars))
    addExps(buf, generateLams(size, vars))
    addExps(buf, generateCases(size, vars))
    val time = System.currentTimeMillis - start
    println("found " + buf.length + " exprs")
    println("genTime: " + time)
    println("checkerTime: " + timeOfChecker)
    println("bufferTime: " + timeInBuf)
    //for(e <- buf) {
      //assert(TermAlgebra.size(e) == size, "size(" + e +") != " + size)
    //}
    buf
  }
  
  private def generateRecAll(size: Int, vars: List[Variable]): Buffer[Expression] = {
    val buf = new ArrayBuffer[Expression]()
    if (size == 0) {
      return buf
    }
    addExps(buf, generateApp(size, vars))
    addExps(buf, generateVars(size, vars))
    addExps(buf, generateCtrs(size, vars))
    addExps(buf, generateCases(size, vars))
    buf
  }
  
  private def generateRecWOCtrs(size: Int, vars: List[Variable]): Buffer[Expression] = {
    val buf = new ArrayBuffer[Expression]()
    if (size == 0) {
      return buf
    }
    addExps(buf, generateApp(size, vars))
    addExps(buf, generateVars(size, vars))
    addExps(buf, generateCases(size, vars))
    buf
  }
  
  private def generateRecWOCtrsLVars(size: Int, vars: List[Variable]): Buffer[Expression] = {
    val buf = new ArrayBuffer[Expression]()
    if (size == 0) {
      return buf
    }
    addExps(buf, generateApp(size, vars))
    addExps(buf, generateGVars(size, vars))
    addExps(buf, generateCases(size, vars))
    buf
  }
  
  private def generateRecWOCtrsGVars(size: Int, vars: List[Variable]): Buffer[Expression] = {
    val buf = new ArrayBuffer[Expression]()
    if (size == 0) {
      return buf
    }
    addExps(buf, generateApp(size, vars))
    addExps(buf, generateLVars(size, vars))
    addExps(buf, generateCases(size, vars))
    buf
  }
  
  var timeInBuf: Long = 0
  
  private def addExps(buf: Buffer[Expression], exps: Buffer[Expression]): Unit = {
    for (exp <- exps) {
      try {
        typeCheck(exp)
        val start = System.currentTimeMillis
        buf += exp
        timeInBuf += System.currentTimeMillis - start
      } catch {
        case e =>
      }
    }
  }

  // only global vars
  private def generateGVars(size: Int, vars: List[Variable]): Buffer[Expression] = {
    val res = new ArrayBuffer[Expression]()
    size match {
      case 1 => { 
        res ++= globals
      }
      case _ =>
	}
    res
  }
  
  // only local vars
  private def generateLVars(size: Int, vars: List[Variable]): Buffer[Expression] = {
    val res = new ArrayBuffer[Expression]()
    size match {
      case 1 => { 
        res ++= vars
      }
      case _ =>
	}
    res
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
      val e1s = generateRecWOCtrsLVars(i, vars)
      val e2s = generateRecAll(size - i, vars)
      for (e1 <- e1s; e2 <- e2s) {
         res += Application(e1, e2)
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
    val bodies = generateRecAll(size - 1, extVars)
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
        if (!branchLists.isEmpty) {
          val selectors = generateRecWOCtrsGVars(i, vars)
          for (selector <- selectors) {
            for (branchList <- branchLists) {
              res += CaseExpression(selector, branchList)
            }
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
      case Nil => {
        if (totalExpSize == 0) {
          res += Nil  
        }
      }
      case d :: ds => {
        val ptVars = d.args map {_ => TermAlgebra.newVar()}
        val pt = Pattern(d.name, ptVars)
        for (i <- 1 to (totalExpSize - ds.size)) {
          val otherBranchesS = generateBranches(ds, totalExpSize - i, vars)
          if (!otherBranchesS.isEmpty)  { 
            val bodies: Buffer[Expression] = generateRecAll(i, ptVars ::: vars)
            for (body <- bodies) {
              val branch = Branch(pt, body)
              for (otherBranches <- otherBranchesS) {
                res += branch :: otherBranches
              }
            }
          }
        }
      }
    }
    res
  }
  
  // generate lists of size listSize where total size of expressions = totalExpSize
  private def generateList(listSize: Int, totalExpSize: Int, vars: List[Variable]): Buffer[List[Expression]] = {
    assert(totalExpSize >= listSize)
    val res = new ArrayBuffer[List[Expression]]()
    if (listSize == 0) {
      if (totalExpSize == 0) {
        res += Nil
      }
    } else {
      for (i <- 1 to (totalExpSize - listSize + 1)) {
        val tails: Buffer[List[Expression]] = generateList(listSize - 1, totalExpSize - i, vars)
        if (!tails.isEmpty) {
          val heads: Buffer[Expression] = generateRecAll(i, vars)
          for (head <- heads; tail <- tails) {
            res += (head :: tail)
          }
        }
      }
    }
    res
  }
  
  var timeOfChecker: Long = 0
  
  private def typeCheck(exp: Expression): Expression = {
    val start = System.currentTimeMillis
    val p1 = Program(program.ts, exp, program.fs)
    val e1 = LangUtils.hl0ToELC(p1)
    typeInferrer.inferType(e1)
    timeOfChecker += System.currentTimeMillis - start
    exp
  }
}
