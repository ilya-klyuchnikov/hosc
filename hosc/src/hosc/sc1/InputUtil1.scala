package hosc.sc1

import scala.util.parsing.input.{CharArrayReader, Reader, StreamReader}
import java.io.{BufferedReader, File, FileReader}
import LangUtils._
import HLanguage1._

object InputUtil1 {
  def program1FromString(input: String): Program1 = {
    val pr = HParsers1.parseProgram(new CharArrayReader(input.toCharArray))
    if (pr.successful) {
      val program = pr.get
      val canExpr = canonize1(program.expr)      
      val canProgram = Program1(program.ts, canExpr)
      Postprocessor1.postprocess(canProgram)
      val elcExpr = LangUtils.hl1ToELC(canExpr)
      val typeInferrer = new TypeInferrer(canProgram.ts)
      typeInferrer.inferType(elcExpr)
      canProgram
    } else { 
      throw new IllegalArgumentException(pr.toString)
    }
  }
  
  def program1FromFile(fileName: String) = {
    val file = new File(fileName)
    val pr = HParsers1.parseProgram(StreamReader(new FileReader(fileName)))
    if (pr.successful) {
      val program = pr.get
      val canExpr = canonize1(program.expr)      
      val canProgram = Program1(program.ts, canExpr)
      Postprocessor1.postprocess(canProgram)
      val elcExpr = LangUtils.hl1ToELC(canExpr)
      val typeInferrer = new TypeInferrer(canProgram.ts)
      typeInferrer.inferType(elcExpr)
      canProgram
    } else {
      println(pr)
      throw new IllegalArgumentException(pr.toString)
    }
  }
}
