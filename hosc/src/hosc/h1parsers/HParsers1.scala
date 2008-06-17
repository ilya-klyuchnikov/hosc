package hosc.h1parsers;

import scala.util.parsing.input.{Positional, Reader}
import scala.util.parsing.combinator.ImplicitConversions
import HLanguage1._

object HParsers1 extends HTokenParsers1 with StrongParsers with ImplicitConversions {
  
  lexical.delimiters += ("(", ")", ",", "=", ";", ":", "%", "{", "}", "::", "|", "[", "]")
  lexical.reserved += ("case", "of", "where", "letrec", "in", "Repeat", "Loop")
  
  def term: Parser[Term1] = (("Repeat" ~ ":")~> term0 ^^ {case t => t.label = Repeat(); t}) | 
                            (("Loop" ~ ":") ~> term0 ^^ {case t => t.label = Loop(); t}) | 
                            term0
  def term0: Parser[Term1] = tr2 | appl | letrec | failure("term is expected")
  def appl = chainl1(tr0, tr1, success(Application1(_: Term1, _: Term1)))
    
  // head of application
  private def tr0: Parser[Term1] = variable | lambdaAbstraction | caseExpression |("(" ~> appl <~ ")")
  // argument of or application constructor
  private def tr1 = tr0 | uident ^^ {x => Constructor1(x, Nil)} | ("(" ~> term <~ ")")
  // top constructor; cannot be head of application
  private def tr2: Parser[Constructor1] =  uident ~ (tr1*) ^^ Constructor1 | ("(" ~> tr2 <~ ")")
  
  private def variable = ((lident | fident | sident) ^^ Variable1) | 
                         (("[" ~> (lident | fident | sident) <~ "]") ^^ {case name => val v = Variable1(name); v.call = true; v})  
  private def lambdaAbstraction = "%" ~> variable ~ ("{" ~> term <~ "}") ^^ LambdaAbstraction1    
  private def caseExpression = "case" ~> term ~ ("of" ~> "{"~> (branch+) <~ "}") ^^ CaseExpression1
  private def branch = pattern ~ (":" ~> term <~ ";") ^^ Branch1  
  private def pattern = uident ~ (variable*) ^^ Pattern1
  
  private def letrec = ("letrec" ~> variable) ~ ("=" ~> term) ~ ("in" ~> term) ^^ 
                       {case v ~ l ~ e => LetRecExpression1((v, l), e)}
  
  def parseTerm(r: Reader[Char]) = strong(term, "<eof> expected") (new lexical.Scanner(r))  
}