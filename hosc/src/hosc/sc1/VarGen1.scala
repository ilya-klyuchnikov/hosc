package hosc.sc1

import HLanguage1._

class VarGen1 {
  var i = 0
  
  def createFreshVar() = {
    i += 1
    Variable1("$v" + i)
  }
  
  def createFreshLetrecVar() = {
    i += 1
    val freshLetrecVar = Variable1("$f" + i)
    freshLetrecVar.call = true
    freshLetrecVar
  }
}
