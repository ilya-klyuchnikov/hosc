package hosc

import scala.util.parsing.input.{CharArrayReader, Reader}

import HLanguage._
import TermAlgebra._

class Interpreter(program: Program) {
  def this(fileName: String) = this(Util.programFromFile(fileName))
  
  def eval(t: Term): Term = {
    val lazyResult = lazyEval(t) 
    Constructor(lazyResult.name, lazyResult.args.map(eval))
  }
  
  def eval(input: String): Term = {
    val pr = HParsers.parseTerm(new CharArrayReader(input.toCharArray))
    if (pr.isEmpty) throw new IllegalArgumentException(pr.toString)
    val term = pr.get
    Validator.valTerm(Set.empty[String] ++ (program.fs map {f => f.name}), term, program)
    Postprocessor.process(term, Set.empty[Variable])
    eval(term)
  }
  
  // TODO
  private def lazyEval(t: Term): Constructor = null
  
}
