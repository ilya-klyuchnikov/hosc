package hosc

import hosc.ProcessTree._

object ProcessTreeAlgebra {
  def size(tree: ProcessTree): Int = {
    nodeSize(tree.rootNode)
  }
  
  def nodeSize(node: Node): Int = {
    var result = 1
    for (chNode <- node.children()) {
      result += nodeSize(chNode)
    }
    result
  }
}
