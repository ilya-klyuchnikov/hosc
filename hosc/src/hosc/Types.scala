package hosc;

import scala.util.parsing.input.Positional

sealed abstract class Type extends Positional
case class TypeVariable(name: String) extends Type {
  override def toString = name
}
case class TypeConstructor(name: String, typeParameters: List[Type]) extends Type {
  override def toString = typeParameters match {
    case Nil => name
    case _   => "(" + name + " " + typeParameters.mkString(" ") + ")";
  }
}
case class Arrow(t1: Type, t2: Type) extends Type {pos = t1.pos}
 
abstract sealed class TypeDefinition extends Positional {
  def name: String
}
case class TypeConstructorDefinition(name: String, args: List[TypeVariable], cons: List[DataConstructor])
  extends TypeDefinition {
  override def toString = name + " " + args.mkString(" ") + " :: " + cons.mkString(" | ") + ";";     
}
case class ArrowDefinition(name: String, ac: Arrow) extends TypeDefinition
case class DataConstructor(name: String, args: List[Type]) extends Positional {
  override def toString = args match {
    case Nil => name
    case _   => "(" + name + " " + args.mkString(" ") + ")";
  }
}