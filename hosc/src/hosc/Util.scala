package hosc;

import scala.util.parsing.input.{CharArrayReader, Reader}
import java.io.{BufferedReader, File, FileReader}
import HLanguage._
import LangUtils._
import sc0.HParsers0

object Util {
  def programFromFile(fileName: String): Program = {
    val file = new File(fileName)
    val sb = new StringBuilder
    val in = new BufferedReader(new FileReader(fileName));
    var str: String = null
    do {
      str = in.readLine
      if (str != null){
        sb.append(str)
        sb.append("\n")
      }
    } while (str != null)
    in.close();
    val pr = HParsers0.parseProgram(new CharArrayReader(sb.toString.toCharArray))
    if (pr.successful) {
      val program = pr.get
      val program1 = LambdaLifting.lift(program)
      val ti = new TypeInferrer(program1.ts)
      ti.inferType(hl0ToELC(program1))
      program1
    } else { 
      throw new IllegalArgumentException(pr.toString)
    }
  }
  
  def programFromString(input: String): Program = {
    val pr = HParsers0.parseProgram(new CharArrayReader(input.toCharArray))
    if (pr.successful) {
      val program = pr.get
      val ti = new TypeInferrer(program.ts)
      ti.inferType(hl0ToELC(program))
      program
    } else { 
      throw new IllegalArgumentException(pr.toString)
    }
  }
  
  def rawProgramFromFile(fileName: String): Program = {
    val file = new File(fileName)
    val sb = new StringBuilder
    val in = new BufferedReader(new FileReader(fileName));
    var str: String = null
    do {
      str = in.readLine
      if (str != null){
        sb.append(str)
        sb.append("\n")
      }
    } while (str != null)
    in.close();
    val pr = HParsers0.parseProgram(new CharArrayReader(sb.toString.toCharArray))
    if (pr.successful) {
      pr.get
    } else { 
      throw new IllegalArgumentException(pr.toString)
    }    
  }
}
