package hosc

import Util._
import HLanguage._
import LangUtils._
import hosc.lemmas.LemmaGenerator

object TestLemmas {
  def main(args : Array[String]) : Unit = {
    val p = programFromFile("sc/lemmas.hs");
    val lg = LemmaGenerator(p)
    val vrbs = List("f") map {Variable(_)}
    lg.genAllExprs(4, vrbs)
    val exps = lg.genAllExprs(4, vrbs)
    
    // 1) eliminates wrong-types exps
    var typedEs: List[Expression] = Nil
    var e2type: Map[Expression, Type] = Map()
    val ti = new TypeInferrer(p.ts)
    
    for (e <- exps) {
    	val p1 = Program(p.ts, e, p.fs)
    	try {
    	  val t = ti.inferType(hl0ToELC(p1))
    	  typedEs = e :: typedEs
    	  e2type = e2type + (e -> t)
    	} catch {
    	  case e =>
    	}
    }
    typedEs = typedEs.reverse
    
    var typedLemmas: List[(Expression, Expression)] = Nil;
    var proved: List[(Expression, Expression)] = Nil;
    for (e1 <- typedEs; e2 <- typedEs) {
      if (e1 != e2 && TypeAlgebra.equivalent(e2type(e1), e2type(e2))) {
        //println((e1, e2))
        val p1 = Program(p.ts, e1, p.fs)
        val p2 = Program(p.ts, e2, p.fs)
        val scp1 = supercompile(p1)
        val scp2 = supercompile(p2)
        if (Eq.equivalent(scp1.goal, scp2.goal)) {
         proved = (e1, e2) :: proved
         //println((e1, e2))
         println(e1); println(e2); println()
        }
      }
    }
    
    null
  }
  
  def supercompile(program: Program) = {
    val sc = new SuperCompiler(program)
    val pt = sc.buildProcessTree(program.goal)
    val g = new CodeConstructor(program, pt, true)
    val p = g.generateProgram()
    p
  }

}
