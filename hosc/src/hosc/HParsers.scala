package hosc

import scala.util.parsing.input.{Positional, Reader}
import scala.util.parsing.combinator.ImplicitConversions
import HLanguage._

object HParsers extends HTokenParsers with StrongParsers with ImplicitConversions {
  
  lexical.delimiters += ("(", ")", ",", "=", ";", ":", "%", "{", "}", "->", "::", "|")
  lexical.reserved += ("case", "of")
  
  def function = p(lident ~ ("=" ~> lambdaAbstraction) ^^ Function)
  
  def term: Parser[Term] = p(tr2 | appl)
  def appl = chainl1(tr0, tr1, success(Application(_: Term, _: Term)))
    
  // head of application
  private def tr0: Parser[Term] = p(variable | lambdaAbstraction | caseExpression |("(" ~> appl <~ ")"))
  // argument of or application constructor
  private def tr1 = p(tr0 | uident ^^ {x => Constructor(x, Nil)} | ("(" ~> term <~ ")"))
  // top constructor; cannot be head of application
  private def tr2: Parser[Constructor] =  p(uident ~ (tr1*) ^^ Constructor | ("(" ~> tr2 <~ ")"))
  
  private def variable = p(lident ^^ Variable)  
  private def lambdaAbstraction = p("%" ~> variable ~ ("{" ~> term <~ "}") ^^ LambdaAbstraction)    
  private def caseExpression = p("case" ~> term ~ ("of" ~> "{"~> (branch*) <~ "}") ^^ CaseExpression)
  private def branch = p(pattern ~ (":" ~> term <~ ";") ^^ Branch)  
  private def pattern = p(uident ~ (variable*) ^^ Pattern)
  
  def parseTerm(r: Reader[Char]) = strong(term, "<eof> expected") (new lexical.Scanner(r))
  
  
  def typeDefinition: Parser[TypeDefinition] = p(typeConstrDefinition | arrowDefinition)  
  private def typeConstrDefinition = p(lident ~ (typeVariable*) ~ ("::" ~> rep1sep(dataConstructor, "|") <~ ";") ^^
    TypeConstructorDefinition)
    
  private def arrowDefinition = p(lident ~ ("::" ~> arrow <~ ";") ^^ ArrowDefinition)
  
  private def tp1: Parser[Type] = p(lident ^^ {i => TypeConstructor(i, Nil)} | typeVariable | ("(" ~> `type` <~")"))
  private def tp2: Parser[Type] = p(typeVariable | lident ~ (tp1*) ^^ TypeConstructor)
  private def tp3: Parser[Type] = p(tp2 |  ("(" ~> `type` <~ ")"))
  private def `type` = {
    val c = {(x: Type, y: Type) => if (y == null) x else Arrow(x, y)}
    chainr1(tp3, "->" ^^^ c, c, null)
  }
  private def arrow: Parser[Arrow] = p(tp2 ~ ("->" ~> `type`) ^^ Arrow) | ("(" ~> arrow <~ ")") 
  private def typeVariable = p(sident ^^ TypeVariable)  
  private def dataConstructor = p(uident ~ (tp1*) ^^ {case n ~ a => DataConstructor(n, a)})
  
  def program = (typeDefinition*) ~ (function+) ^^ Program
  def parseType(r: Reader[Char]) = strong(`type`, "<eof> expected") (new lexical.Scanner(r))
  def parseProgram(r: Reader[Char]) = validate(strong(program, "<eof> expected") (new lexical.Scanner(r)))
  
  def validate(pr: ParseResult[Program]) = pr match {
    case n: NoSuccess => n;
    case s @ Success(_, _) => Validator.validate(s)
  }
  
  def p[T <: Positional](p: => Parser[T]): Parser[T] = positioned(p)
  
  def error(msg: String, pos: Positional) = {
    lastNoSuccess = null; val e = HError(msg, pos);  lastNoSuccess = null; e
  }
  
  case class HError(override val msg: String, val pos: Positional) extends Error(msg, null) {
    override def toString = "[" + pos.pos +"] error: "+msg+"\n\n"+pos.pos.longString
  }
}