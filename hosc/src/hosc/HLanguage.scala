package hosc;

import scala.util.parsing.input.Positional

/** 
 * Abstract syntax for simple higher-order language
 */
object HLanguage {
   sealed abstract class Expression extends Positional
   sealed abstract class BaseExpression extends Expression
   
   sealed abstract class Term extends BaseExpression
   case class Variable(name: String) extends Term {
     var global = false // global var is call
     override def toString = name 
   } 
   case class Constructor(name: String, args: List[Term]) extends Term {
     override def toString = "(" + name + " " + args.mkString(" ") + ")"
   }
   case class LambdaAbstraction(v: Variable, t: Term) extends Term {
     override def toString = "%" + v.name + " {" + t + "}" 
   }
   case class Application(head: Term, arg: Term) extends Term {
     pos = head.pos
     override def toString = "(" + head + " " + arg + ")"
   } 
   case class CaseExpression(selector: Term, branches: List[Branch]) extends Term {
     override def toString = "case (" + selector + ") of " + branches.mkString("{", " ", "}")  
   }
   case class LetExpression(bs: List[Pair[Variable, Expression]], expr: Expression) extends BaseExpression {
     override def toString = "(let " + (bs map {p => p._1 + "=" + p._2}).mkString(", ") + "\n in " + expr + ")"  
   }
   case class LetRecExpression(bs: List[Pair[Variable, Expression]], expr: Expression) extends Expression {
     override def toString = "(letrec " + (bs map {p => p._1 + "=" + p._2}).mkString(", ") + "\n in " + expr + ")" 
   }
   
   case class Branch(pattern: Pattern, term: Term) extends Positional {
     override def toString = pattern + " : " + term + ";"
   }
   case class Pattern(name: String, args: List[Variable]) extends Positional {
     override def toString = name + " " + args.mkString(" ")  
   }
   
   case class Function(name: String, lam: LambdaAbstraction) extends Positional {
     //var `type` : Type = null
   }
   
   case class Program(ts: List[TypeDefinition], goal: Term, fs: List[Function]) {
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
