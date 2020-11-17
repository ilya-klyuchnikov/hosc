package hosc;

import java.io.{File, FileReader}

import hosc.HLanguage._
import hosc.LangUtils._

import scala.util.parsing.input.StreamReader

object Util {
  def programFromFile(fileName: String): Program = {
    val in = new FileReader(new File(fileName));
    val pr = HParsers.parseProgram(StreamReader(in))
    if (pr.successful) {
      val program = pr.get
      val program1 = LambdaLifting.lift(program)
      val ti = new TypeInferrer(program1.ts)
      ti.inferType(normalize(program1))
      program1
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
      ti.inferType(normalize(program1))
    } else {
      throw new IllegalArgumentException(pr.toString)
    }
  }
}
