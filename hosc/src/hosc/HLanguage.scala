package hosc;

import scala.util.parsing.input.Positional

/** 
 * Abstract syntax for simple higher-order language
 */
object HLanguage {
   sealed abstract class Expression extends Positional
   
   sealed abstract class Term extends Expression
   case class Variable(name: String) extends Term
   case class Constructor(name: String, args: List[Term]) extends Term
   case class LambdaAbstraction(v: Variable, t: Term) extends Term
   case class Application(head: Term, arg: Term) extends Term {pos = head.pos}
   case class CaseExpression(selector: Term, branches: List[Branch]) extends Term
   case class LetExpression(bs: List[Pair[Variable, Expression]], expr: Expression) extends Expression
   case class LetRecExpression(bs: List[Pair[Variable, Expression]], expr: Expression) extends Expression
   
   case class Branch(pattern: Pattern, term: Term) extends Positional
   case class Pattern(name: String, args: List[Variable]) extends Positional
   
   case class Function(name: String, lam: LambdaAbstraction) extends Positional
   
   sealed abstract class Type extends Positional
   case class TypeVariable(name: String) extends Type
   case class TypeConstructor(name: String, typeParameters: List[Type]) extends Type
   case class Arrow(t1: Type, t2: Type) extends Type {pos = t1.pos}

   abstract sealed class TypeDefinition extends Positional
   case class TypeConstructorDefinition(name: String, args: List[TypeVariable], cons: List[DataConstructor])
     extends TypeDefinition   
   case class ArrowDefinition(name: String, ac: Arrow)
     extends TypeDefinition
   case class DataConstructor(name: String, args: List[Type]) extends Positional
   
   case class Program(ts: List[TypeDefinition], fs: List[Function])
}
