package hosc;

import HLanguage._
import HLanguage1._
import TermAlgebra1._
import sc1.ProcessTree1
import sc1.ProcessTree1SVG
import sc1.ProcessTree1._
import HLUtils._
import hosc.util.Canonizer._

class SuperCompiler1_Old(program: Program) {
  val debug = false
  var i = 0
  val scProgram: Program = {
    val pt = new SuperCompiler(program).buildProcessTree(program.goal)
    val generator = new ResidualProgramGenerator(program, pt)
    val p1 = hl1ToHl(generator.generateProgram())
    Postprocessor.postprocess(p1)
    p1
    canonize(p1)
    canonize(program)
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
      continue = transformer.transform()
      dumpTree(tree, "transformed");
      continue = continue | folder.fold()
      dumpTree(tree,  "folded");
    } while (continue);
    
    // 2
    dumpTree(tree,  "before_un");  
    if (ungeneralizer.ungeneralize()){
      dumpTree(tree,  "ungeneralized");
      continue = false
      do {
        continue = transformer.transform()
        dumpTree(tree, "transformed");
        continue = continue | folder.fold()
        dumpTree(tree, "folded");
      } while (continue)
    }
    
    tree
    
  }
  
  def dumpTree(tree: ProcessTree1, s: String) = {
    
    if (debug){
    
    i = i+1
    
    val svg = new ProcessTree1SVG(tree).treeToSVG
    val str = if (i<10) "0" + i else "" + i; 
    val svgFile = new java.io.File("tmp/" + str + s+".svg")
    if (!svgFile.exists){
      svgFile.createNewFile()
    } 
    scala.xml.XML.save("tmp/" + str + s+".svg", svg)
    println(i)
    println(s)}
  }
  
}
