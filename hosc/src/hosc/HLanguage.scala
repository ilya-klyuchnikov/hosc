package hosc;

/** 
 * Abstract syntax for simple higher-order language
 */
object HLanguage {
   sealed abstract class Expression
   
   sealed abstract class Term extends Expression
   case class Variable(name: String) extends Term
   case class Constructor(name: String, args: List[Term]) extends Term
   
   case class CaseExpression(selector: Term, branches: List[Branch]) extends Term   
   case class Call(name: String) extends Term
   case class Application(head: Term, arg: Term) extends Term
   
   case class Branch(pattern: Pattern, term: Term)
   case class Pattern(name: String, args: List[Variable])
}
