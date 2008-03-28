package hosc

import scala.util.parsing.input.Reader

import HLanguage._

object HParsers extends HTokenParsers with StrongParsers {
  
  lexical.delimiters += ("(", ")", ",", "=", ";", ":", "%", "{", "}")
  lexical.reserved += ("case", "of")
  
  private def aterm: Parser[Term] = variable | constructor | lambdaAbstraction | caseExpression |  ("(" ~> term <~ ")")
  
  def term: Parser[Term] = chainl1(aterm, success(Application(_: Term, _: Term)))
  
  private def variable: Parser[Variable] = 
   lident ^^ Variable
  
  private def lambdaAbstraction: Parser[LambdaAbstraction] =
    "%" ~> variable ~ ("{" ~> aterm <~ "}") ^^ {case v ~ t => LambdaAbstraction(v, t)}
    
  private def caseExpression = 
    "case" ~> aterm ~ ("of" ~> "{"~> (branch*) <~ "}") ^^ {case s ~ bs => CaseExpression(s, bs)}
  
  private def branch: Parser[Branch] = 
    pattern ~ (":" ~> aterm <~ ";") ^^ {case p ~ t => Branch(p, t)}
  
  private def pattern: Parser[Pattern] =
    uident ~ (variable*) ^^ {case name ~ args => Pattern(name, args)}

  private def constructor: Parser[Constructor] =
    uident ~ (aterm*) ^^ {case name ~ args => Constructor(name, args)}
  
  def parseTerm(r: Reader[Char]): ParseResult[Term] = strong(term, "term or <eof> expected") (new lexical.Scanner(r))
}
