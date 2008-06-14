package hosc;

import HLanguage._
import HLanguage1._
import TermAlgebra1._
import ProcessTree1._
import HLUtils._

class SuperCompiler1(program: Program) {
  val scProgram: Program = {
    val pt = new SuperCompiler(program).buildProcessTree(program.goal)
    val generator = new ResidualProgramGenerator(program, pt)
    val p1 = hl1ToHl(generator.generateProgram())
    Postprocessor.postprocess(p1)
    p1
    program
  }
  val emptyMap = Map[Variable1, Term1]()
  
  def superCompile() = {
    val tree = ProcessTree1(hlToHl1(scProgram.goal))
    val transformer = new Transformer(tree, scProgram)
    val constructor = new CodeConstructor(tree)
    val folder = new Folder(tree, constructor) 
    val ungeneralizer = new Ungeneralizer(tree, constructor)
    var continue = false
    do {
      continue = transformer.transform() | folder.fold()
    } while (continue)
    if (ungeneralizer.ungeneralize()){
      continue = false
      do {
        continue = transformer.transform() | folder.fold()
      } while (continue)
    }
    
    tree
    
  }
  
}
