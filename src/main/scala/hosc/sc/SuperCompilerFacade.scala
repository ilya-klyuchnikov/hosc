package hosc.sc

import hosc.CodeConstructor
import hosc.HLanguage._
import hosc.Util

trait SuperCompilerFacade {
  def createSuperCompiler(program: Program): SuperCompiler
  
  def superCompile(program: Program): Program = {
    val sc = createSuperCompiler(program)
    val tree = sc.buildProcessTree(program.goal)
    val codeConstructor = new CodeConstructor(program, tree, true)
    val superCompiledProgram = codeConstructor.generateProgram
    superCompiledProgram
  }
  
  def superCompileFile(inputPath: String): Program = {
    val program = Util.programFromFile(inputPath)
    superCompile(program)
  }
  
  def superCompileFile2File(inPath: String, outPath: String): Unit = {
    import java.io.{File, FileWriter}
    val supercompiledProgram = superCompileFile(inPath)
    val outFile = new File(outPath)
    outFile.createNewFile
    val fw = new FileWriter(outFile);
    fw.write("-- generated from " + inPath + "\n")
    supercompiledProgram.toDoc.format(120, fw)
    fw.flush
    fw.close
  }
}