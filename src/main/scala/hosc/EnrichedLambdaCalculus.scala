package hosc

// BOOK - p 39, - Enriched lambda calculus - yay!!!!
object EnrichedLambdaCalculus {
   sealed trait Expression
   case class Variable(name: String) extends Expression {
     override def toString: String =
      name
   }
   case class Constructor(name: String, args: List[Expression]) extends Expression {
     override def toString: String =
      "(" + name + " " + args.mkString(" ") + ")"
   }
   case class LambdaAbstraction(v: Variable, t: Expression) extends Expression {
     override def toString: String =
      "\\" + v.name + " -> (" + t + ")"
   }
   case class Application(head: Expression, arg: Expression) extends Expression {
     override def toString: String =
      "(" + head + " " + arg + ")"
   }
   case class CaseExpression(selector: Expression, branches: List[Branch]) extends Expression {
     override def toString: String =
      "case (" + selector + ") of " + branches.mkString("{", " ", "}")
   }
   case class LetExpression(bs: List[(Variable, Expression)], expr: Expression) extends Expression {
     override def toString: String =
      "let " + (bs map { p => p._1.toString + "=" + p._2.toString}).mkString(", ") + "\n in " + expr
   }
   case class LetRecExpression(bs: List[(Variable, Expression)], expr: Expression) extends Expression {
     override def toString: String =
      "(letrec " + (bs map { p => p._1.toString + "=" + p._2.toString}).mkString(", ") + "\n in " + expr + ")"
   }
   case class Branch(pattern: Pattern, term: Expression) {
     override def toString: String =
      pattern.toString + " -> " + term.toString + ";"
   }
   case class Pattern(name: String, args: List[Variable]) {
     override def toString: String =
      name + " " + args.mkString(" ")
   }
}

sealed trait Type
case class TypeVariable(name: String) extends Type
case class TypeConstructor(name: String, typeParameters: List[Type]) extends Type
case class Arrow(t1: Type, t2: Type) extends Type

sealed trait TypeDefinition {
  def name: String
}
case class TypeConstructorDefinition(name: String, args: List[TypeVariable], cons: List[DataConstructor])
  extends TypeDefinition
case class DataConstructor(name: String, args: List[Type])
