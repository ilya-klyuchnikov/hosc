package hosc;

//import HLanguage._
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
  }
  val emptyMap = Map[Variable1, Term1]()
  
  def superCompile() = {
    val tree = ProcessTree1(hlToHl1(scProgram.goal))
    val transformer = new Transformer(tree, scProgram)
    transformer.transform()
    tree
  }
  
}
