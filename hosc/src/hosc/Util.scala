package hosc;

import scala.util.parsing.input.{CharArrayReader, Reader}
import java.io.{BufferedReader, File, FileReader}
import HLanguage._

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
    val pr = HParsers.parseProgram(new CharArrayReader(sb.toString.toCharArray))
    if (pr.successful) {
      val program = pr.get
      val ti = new TypeInferrer(program)
      ti.tcProgram()
      program
    } else { 
      throw new IllegalArgumentException(pr.toString)
    }
  }
  
  def groundTermFromString(input: String, program: Program) = {
    val pr = HParsers.parseTerm(new CharArrayReader(input.toCharArray))
    if (pr.isEmpty) throw new IllegalArgumentException(pr.toString)
    val term = pr.get
    Validator.valTerm(Set.empty[String] ++ (program.fs map {f => f.name}), term, program)
    val globals = Set[Variable]() ++ (program.fs map (f => Variable(f.name)))
    Postprocessor.process(term, globals)
    val ti = new TypeInferrer(program)
    ti.tcGroundTerm(term)
    term
  }
  
  def termFromString(input: String, program: Program) = {
    val pr = HParsers.parseTerm(new CharArrayReader(input.toCharArray))
    if (pr.isEmpty) throw new IllegalArgumentException(pr.toString)
    val term = pr.get
    Validator.valTermWithFreeVars(Set.empty[String] ++ (program.fs map {f => f.name}), term, program)
    val globals = Set[Variable]() ++ (program.fs map (f => Variable(f.name)))
    Postprocessor.process(term, globals)
    val ti = new TypeInferrer(program)
    ti.tcTerm(term)
    term
  }
  
  def typeForGroundTerm(input: String, program: Program) = {
    val pr = HParsers.parseTerm(new CharArrayReader(input.toCharArray))
    if (pr.isEmpty) throw new IllegalArgumentException(pr.toString)
    val term = pr.get
    Validator.valTerm(Set.empty[String] ++ (program.fs map {f => f.name}), term, program)
    Postprocessor.process(term, Set.empty[Variable])
    val ti = new TypeInferrer(program)
    ti.tcGroundTerm(term)
  }
  
  def typeForTerm(input: String, program: Program) = {
    val pr = HParsers.parseTerm(new CharArrayReader(input.toCharArray))
    if (pr.isEmpty) throw new IllegalArgumentException(pr.toString)
    val term = pr.get
    Validator.valTerm(Set.empty[String] ++ (program.fs map {f => f.name}), term, program)
    Postprocessor.process(term, Set.empty[Variable])
    val ti = new TypeInferrer(program)
    ti.tcTerm(term)
  }
}
