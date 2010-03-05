package hosc.sc

import hosc.HLanguage._
import hosc.ProcessTree._

class SuperCompiler(val program: Program) extends SuperCompilerAlgorithm with Driver with TreeOperations {
  val renaming  = RenamingOrdering
  def embedding = EmbeddingOrdering
  def instance  = InstanceOrdering
}

class SuperCompilerWithControl(val program: Program) extends SuperCompilerAlgorithm with Driver with TreeOperations {
  val renaming  = RenamingOrdering
  def embedding = EmbeddingOrderingWithControl
  def instance  = InstanceOrderingWithControl
}