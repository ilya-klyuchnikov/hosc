package hosc;

import scala.util.parsing.input.Positional

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