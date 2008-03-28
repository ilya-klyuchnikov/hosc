package hosc;

/** 
 * Abstract syntax for simple higher-order language
 */
object HLanguage {
   sealed abstract class Expression
   
   sealed abstract class Term extends Expression
   case class Variable(name: String) extends Term
   case class Constructor(name: String, args: List[Term]) extends Term
   case class Call(name: String) extends Term
   case class LambdaAbstraction(v: Variable, t: Term) extends Term
   case class Application(head: Term, arg: Term) extends Term
   case class CaseExpression(selector: Term, branches: List[Branch]) extends Term   
   
   case class Branch(pattern: Pattern, term: Term)
   case class Pattern(name: String, args: List[Variable])
   
   case class Function(name: String, lam: LambdaAbstraction)
   
   sealed abstract class Type
   case class TypeVariable(name: String) extends Type
   case class TypeConstructor(name: String, typeParameters: List[Type]) extends Type
   case class Arrow(t1: Type, t2: Type) extends Type

   abstract sealed class TypeDefinition
   case class TypeConstructorDefinition(name: String, args: List[TypeVariable], cons: List[DataConstructor])
     extends TypeDefinition   
   case class ArrowDefinition(name: String, ac: ArrowConstructor)
     extends TypeDefinition
   case class DataConstructor(name: String, args: List[Type])
   case class ArrowConstructor(t1: Type, t2: Type)
   
   case class Program(ts: List[TypeDefinition], fs: List[Function])
}
