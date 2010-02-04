package hosc

import HLanguage._

trait ProcessTreeRenamer {
  def renameVars(p: ProcessTree): ProcessTree = {
    val vars = p.rootNode.getAllVars()
    var i = 0
    def createVar(): Variable = {      
      var nv: Variable = null
      do {
        nv = varFor(i)
        i += 1
      } while (vars contains nv)
      nv
    }
    var map = Map[Variable, Variable]()
    for (v <- vars.toList) {
      if (isSynthetic(v)) {
        map = map + (v -> createVar)
      }
    }
    p.rootNode sub map
    p
  }
  
  private val vNames = "xyzuvwprst".toArray
  
  private def varFor(j: Int) = {
    if (j <= 9) 
      Variable("" + vNames(j))
    else 
      Variable("" + vNames(j % 10) + Integer.toString(j / 10))   
  }
  
  private def isSynthetic(v: Variable) = v.name startsWith "$" 

}
