package hosc;

import scala.util.parsing.input.{CharArrayReader, StreamReader}
import java.io.{File, FileReader}
import HLanguage._
import LangUtils._

object Util {
  def programFromFile(fileName: String): Program = {
    val in = new FileReader(new File(fileName));
    val pr = HParsers.parseProgram(StreamReader(in))
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
  
  def programWithLemmas(pf: String, lf: String): (Program, List[Lemma]) = {
    val program = programFromFile(pf)
    val lemmas = lemmasFromFile(program, lf)
    (program, lemmas)
  }
  
  def lemmasFromFile(program: Program, fileName: String): List[Lemma] = {
    val in = new FileReader(new File(fileName));
    val pr = HParsers.parseLemmasForProgram(program, StreamReader(in))
    if (pr.successful) {
      pr.get
    } else {
      throw new IllegalArgumentException(pr.toString)
    }
  }
  
  def programFromString(input: String): Program = {
    val pr = HParsers.parseProgram(new CharArrayReader(input.toCharArray))
    if (pr.successful) {
      val program = pr.get
      val ti = new TypeInferrer(program.ts)
      ti.inferType(hl0ToELC(program))
      program
    } else { 
      throw new IllegalArgumentException(pr.toString)
    }
  }
  
  def inferGoalType(fileName: String): Type = {
    val in = new FileReader(new File(fileName))
    val pr = HParsers.parseProgram(StreamReader(in))
    if (pr.successful) {
      val program = pr.get
      val program1 = LambdaLifting.lift(program)
      val ti = new TypeInferrer(program1.ts)
      ti.inferType(hl0ToELC(program1))
    } else { 
      throw new IllegalArgumentException(pr.toString)
    }
  }
  
  def rawProgramFromFile(fileName: String): Program = {
    val in = new FileReader(new File(fileName))
    val pr = HParsers.parseProgram(StreamReader(in))
    if (pr.successful) {
      pr.get
    } else { 
      throw new IllegalArgumentException(pr.toString)
    }    
  }
}
