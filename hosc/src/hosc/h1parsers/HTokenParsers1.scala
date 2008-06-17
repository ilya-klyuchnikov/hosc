package hosc.h1parsers;

import scala.util.parsing.combinator.syntactical.StdTokenParsers

class HTokenParsers1 extends StdTokenParsers {
  
  type Tokens = HTokens1
  val lexical = new HLexical1
  
  import lexical.{SIdentifier, UIdentifier, LIdentifier, FIdentifier}
  
  def sident: Parser[String] = 
    elem("identifier", _.isInstanceOf[SIdentifier]) ^^ (_.chars)
    
  def uident: Parser[String] = 
    elem("identifier", _.isInstanceOf[UIdentifier]) ^^ (_.chars)
    
  def lident: Parser[String] = 
    elem("identifier", _.isInstanceOf[LIdentifier]) ^^ (_.chars)  
  def fident: Parser[String] = 
    elem("identifier", _.isInstanceOf[FIdentifier]) ^^ (_.chars) 
}
