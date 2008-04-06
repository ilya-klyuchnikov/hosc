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
 
   case class TypeDefinition(name: String, args: List[TypeVariable], cons: List[DataConstructor])
     extends Positional
   case class DataConstructor(name: String, args: List[Type]) extends Positional
   
   case class Program(ts: List[TypeDefinition], fs: List[Function]) {
     def getTypeDefinition(tName: String): Option[TypeDefinition] = {
       for (td <- ts) if (td.name == tName) return Some(td)
       None
     }
     def getTypeDefinitionForDC(dataConsName: String): Option[TypeDefinition] = {
       for (td <- ts; dc <- td.cons) if (dc.name == dataConsName) return Some(td)
       None
     }
     def getDataConstructor(dataConsName: String): Option[DataConstructor] = {
       for (td <- ts; dc <- td.cons) if (dc.name == dataConsName) return Some(dc)
       None
     }
     def getFunction(n: String): Option[Function] = {
       for (f <- fs) if (f.name == n) return Some(f)
       None
     }
   }
}
