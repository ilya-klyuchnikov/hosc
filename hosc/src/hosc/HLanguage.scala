package hosc;

import scala.util.parsing.input.Positional

/** 
 * Abstract syntax for simple higher-order language
 */
object HLanguage {
   sealed abstract class Expression extends Positional
   
   sealed abstract class Term extends Expression
   case class Variable(name: String) extends Term {
     var global = false // global var is call
     override def toString = name 
   } 
   case class Constructor(name: String, args: List[Term]) extends Term {
     override def toString = "(" + name + " " + args.mkString("") + ")"
   }
   case class LambdaAbstraction(v: Variable, t: Term) extends Term {
     override def toString = "%" + v.name + " {" + t + "}" 
   }
   case class Application(head: Term, arg: Term) extends Term {
     pos = head.pos
     override def toString = "(" + head + " " + arg + ")"
   } 
   case class CaseExpression(selector: Term, branches: List[Branch]) extends Term {
     override def toString = "case (" + selector + ") of {\n" + branches.mkString("\n")+"}\n"  
   }
   case class LetExpression(bs: List[Pair[Variable, Expression]], expr: Expression) extends Expression
   case class LetRecExpression(bs: List[Pair[Variable, Expression]], expr: Expression) extends Expression
   
   case class Branch(pattern: Pattern, term: Term) extends Positional {
     override def toString = pattern + " : " + term + ";"
   }
   case class Pattern(name: String, args: List[Variable]) extends Positional {
     override def toString = name + " " + args.mkString(" ")  
   }
   
   case class Function(name: String, lam: LambdaAbstraction) extends Positional {
     var `type` : Type = null
   }
   
   sealed abstract class Type extends Positional
   case class TypeVariable(name: String) extends Type
   case class TypeConstructor(name: String, typeParameters: List[Type]) extends Type
   case class Arrow(t1: Type, t2: Type) extends Type {pos = t1.pos}
 
   abstract sealed class TypeDefinition extends Positional {
     def name: String
   }
   case class TypeConstructorDefinition(name: String, args: List[TypeVariable], cons: List[DataConstructor])
     extends TypeDefinition
   case class ArrowDefinition(name: String, ac: Arrow) extends TypeDefinition
   case class DataConstructor(name: String, args: List[Type]) extends Positional
   
   case class Program(ts: List[TypeDefinition], fs: List[Function]) {
     def getTypeConstructorDefinition(tcName: String): Option[TypeConstructorDefinition] = {
       for (td <- ts) td match {case tcd: TypeConstructorDefinition if tcName == tcd.name => return Some(tcd); case _ => } 
       None
     }
     def getTypeDefinitionForDC(dataConsName: String): Option[TypeConstructorDefinition] = {
       for (td <- ts) td match {
         case tcd: TypeConstructorDefinition => for (dc <- tcd.cons) if (dc.name == dataConsName) return Some(tcd)
         case _ => 
       }
       None
     }
     def getDataConstructor(dataConsName: String): Option[DataConstructor] = {
       for (td <- ts) td match {
         case tcd: TypeConstructorDefinition => for (dc <- tcd.cons) if (dc.name == dataConsName) return Some(dc)
         case _ => 
       }
       None
     }
     def getFunction(n: String): Option[Function] = {
       for (f <- fs) if (f.name == n) return Some(f)
       None
     }
   }
}
