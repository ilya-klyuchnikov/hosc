package hosc;

import scala.util.parsing.input.Positional

object HLanguage {
   sealed abstract class Expression extends Positional {
     type termType <: Expression
     def \\(s: Map[Variable, Variable]): termType
     def / (s: Map[Variable, Expression]): Expression
   }
   case class Variable(name: String) extends Expression {
     type termType = Variable
     def  \\(s: Map[Variable, Variable]): Variable =
       s.get(this) match {case Some(t) => t; case None => this}
     def / (s: Map[Variable, Expression]): Expression =
       s.get(this) match {case Some(t) => t; case None => this}
     var global = false // global var is call
     override def toString: String =
       name
   }
   case class Constructor(name: String, args: List[Expression]) extends Expression {
     type termType = Constructor
     def \\(s: Map[Variable, Variable]): Constructor =
       Constructor(name, args map {_\\s})
     def / (s: Map[Variable, Expression]): Expression =
       Constructor(name, args map {_/s})
     override def toString: String =
       "(" + name + (args match {case Nil => ""; case _ => args.mkString(" ", " ","")}) + ")"
   }
   case class LambdaAbstraction(v: Variable, t: Expression) extends Expression {
     type termType = LambdaAbstraction
     def \\(s: Map[Variable, Variable]): LambdaAbstraction =
       LambdaAbstraction(v\\s, t\\s)
     def / (s: Map[Variable, Expression]): Expression =
       LambdaAbstraction(v, t/s)
     override def toString: String =
       "(\\" + v.name + " -> " + t + ")"
   }
   case class Application(head: Expression, arg: Expression) extends Expression {
     type termType = Application
     def \\(s: Map[Variable, Variable]): Application =
       Application(head\\s, arg\\s)
     def / (s: Map[Variable, Expression]) = Application(head/s, arg/s)
     pos = head.pos
     override def toString: String =
       "(" + head + " " + arg + ")"
   }
   case class CaseExpression(selector: Expression, branches: List[Branch]) extends Expression {
     type termType = CaseExpression
     def \\(s: Map[Variable, Variable]): CaseExpression =
       CaseExpression(selector\\s, branches map {_\\s})
     def / (s: Map[Variable, Expression]): Expression =
       CaseExpression(selector/s, branches map { b => Branch(b.pattern, b.term / s)})
     override def toString: String =
       "case (" + selector + ") of " + branches.mkString("{", " ", "}")
   }
   case class LetExpression(bs: List[(Variable, Expression)], expr: Expression) extends Expression {
     type termType = LetExpression
     def \\(s: Map[Variable, Variable]): LetExpression =
       LetExpression(bs map { b => (b._1\\s, (b._2\\s).asInstanceOf[Expression])}, expr\\s)
     def / (s: Map[Variable, Expression]): Expression =
       LetExpression(bs map { b => (b._1, b._2/s)}, expr/s)
     override def toString: String =
       "let " + (bs map { p => p._1 + "=" + p._2}).mkString(", ") + "\n in " + expr
   }
   case class LetRecExpression(binding: (Variable, Expression), expr: Expression) extends Expression {
     type termType = LetRecExpression
     def \\(s: Map[Variable, Variable]): LetRecExpression =
       LetRecExpression((binding._1\\s, binding._2\\s), expr\\s)
     def / (s: Map[Variable, Expression]): Expression =
       LetRecExpression((binding._1, binding._2/s), expr/s)
     override def toString: String =
       "(letrec " + (binding._1 + "=" + binding._2) + "\n in " + expr + ")"
   }

   case class Branch(pattern: Pattern, term: Expression) extends Positional {
     def \\(s: Map[Variable, Variable]): Branch = Branch(pattern\\s, term\\s)
     override def toString: String =
       pattern + " -> " + term + ";"
   }
   case class Pattern(name: String, args: List[Variable]) extends Positional {
     def \\(s: Map[Variable, Variable]) = Pattern(name, args map {_\\s})
     override def toString: String =
       name + " " + args.mkString(" ")
   }

   case class Function(name: String, body: Expression) extends Positional

   case class Program(ts: List[TypeConstructorDefinition], goal: Expression, fs: List[Function]) {
     def getTypeConstructorDefinition(tcName: String): Option[TypeConstructorDefinition] = {
       for (td <- ts) td match {case tcd: TypeConstructorDefinition if tcName == tcd.name => return Some(tcd); case _ => }
       None
     }
     def getTypeDefinitionForDC(dataConsName: String): Option[TypeConstructorDefinition] = {
       for (td <- ts)
         for (dc <- td.cons)
           if (dc.name == dataConsName) return Some(td)
       None
     }
     def getDataConstructor(dataConsName: String): Option[DataConstructor] = {
       for (td <- ts)
         for (dc <- td.cons)
           if (dc.name == dataConsName) return Some(dc)
       None
     }
     def getFunction(n: String): Option[Function] = {
       for (f <- fs) if (f.name == n) return Some(f)
       None
     }
   }
}
