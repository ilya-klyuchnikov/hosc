package hosc.sc

import hosc.HLanguage._
import hosc.ProcessTree._

class SuperCompiler(val program: Program) extends SuperCompilerAlgorithm with Driver with TreeOperations {
  val renaming  = RenamingOrdering
  val embedding = EmbeddingOrdering
  val instance  = InstanceOrdering
}

class SuperCompilerWithControl(val program: Program) extends SuperCompilerAlgorithm with Driver with TreeOperations {
  val renaming  = RenamingOrderingWithControl
  val embedding = EmbeddingOrderingWithControl
  val instance  = InstanceOrderingWithControl
}