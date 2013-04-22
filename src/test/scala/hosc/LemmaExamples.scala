package hosc

import hosc.Util._

object LemmaExamples {
  def main(args : Array[String]) : Unit = {
    //step("hl/even_doubleAcc3.hs", "hl/even_doubleAcc4.hs")
    //step("hl/rev2.hs", "hl/rev1.hs")
    step("examples/spec_fhl/lemma1_1.hs", "examples/spec_fhl/lemma1_2.hs")
  }
  
  def step(f1: String, f2: String) : Unit = {
    
    val p1 = supercompile(f1).goal
    val p2 = supercompile(f2).goal
    
    val eq  = Eq.equivalent(p1, p2)
    println(eq)
    println(p1)
    if (TicksAlgebra.isImprovement(p1, p2)) {
      println("<=")
    } else {
      println("!<=")
    }
    println(p2)
    
  }
  
  def supercompile(file: String) = {
    val program = programFromFile(file)
    val sc = new SuperCompiler0(program)
    sc.useControl = false
    val pt = sc.buildProcessTree(program.goal)
    val g = new CodeConstructor(program, pt, true)
    val p = g.generateProgram()
    p
  }
}
