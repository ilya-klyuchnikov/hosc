package hosc.sc1

import scala.util.parsing.input.{Positional, Reader}
import scala.util.parsing.combinator.ImplicitConversions
import HLanguage1._

object HParsers1 extends HTokenParsers with StrongParsers with ImplicitConversions {
  
  lexical.delimiters += ("(", ")", ",", "=", ";", ":", "%", "{", "}", "::", "|", "[", "]", "->")
  lexical.reserved += ("case", "of", "where", "letrec", "in", "Repeat", "Loop")
  
  def term: Parser[Term1] = tr2 | appl | letrec | failure("term is expected")
  def appl = chainl1(tr0, tr1, success(Application1(_: Term1, _: Term1)))
    
  // head of application
  private def tr0: Parser[Term1] = variable | lambdaAbstraction | caseExpression |("(" ~> appl <~ ")")
  // argument of or application constructor
  private def tr1 = tr0 | uident ^^ {x => Constructor1(x, Nil)} | ("(" ~> term <~ ")")
  // top constructor; cannot be head of application
  private def tr2: Parser[Constructor1] =  uident ~ (tr1*) ^^ Constructor1 | ("(" ~> tr2 <~ ")")
  
  private def variable = p(lident ^^ Variable1)   
  private def lambdaAbstraction = "%" ~> c(variable) ~ (c("{") ~> term <~ c("}")) ^^ LambdaAbstraction1    
  private def caseExpression = "case" ~> c(term) ~ (c("of") ~> c("{")~> (branch+) <~ c("}")) ^^ CaseExpression1
  private def branch = pattern ~ (c(":") ~> c(term) <~ c(";")) ^^ Branch1  
  private def pattern = uident ~ (variable*) ^^ Pattern1
  
  private def letrec:Parser[LetRecExpression1] = ("letrec" ~> c(variable)) ~ (c("=") ~> c(term)) ~ (c("in") ~> c(term)) ^^ 
                       {case v ~ l ~ e => LetRecExpression1((v, l), e)} | ("(" ~> letrec <~ ")")
  
  // types
  private def typeConstrDefinition = p(lident ~ (typeVariable*) ~ ("::" ~> rep1sep(dataConstructor, "|") <~ ";") ^^
    TypeConstructorDefinition)
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
    
  def p[T <: Positional](p: => Parser[T]): Parser[T] = positioned(p)
  def c[T](p: => Parser[T]): Parser[T] = commit(p)
  
  def parseTerm(r: Reader[Char]) = strong(term) (new lexical.Scanner(r))
  def program1 = (typeConstrDefinition*) ~ term  ^^ Program1
  def parseProgram(r: Reader[Char]) = strong(program1) (new lexical.Scanner(r))
}