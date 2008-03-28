package hosc

import scala.util.parsing.combinator.syntactical.StdTokenParsers

class HTokenParsers extends StdTokenParsers {
  
  type Tokens = HTokens
  val lexical = new HLexical
  
  import lexical.{SIdentifier, UIdentifier, LIdentifier}
  
  def sident: Parser[String] = 
    elem("identifier", _.isInstanceOf[SIdentifier]) ^^ (_.chars)
    
  def uident: Parser[String] = 
    elem("identifier", _.isInstanceOf[UIdentifier]) ^^ (_.chars)
    
  def lident: Parser[String] = 
    elem("identifier", _.isInstanceOf[LIdentifier]) ^^ (_.chars)  
  
}
