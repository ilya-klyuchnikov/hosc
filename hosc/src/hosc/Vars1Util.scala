package hosc;

import HLanguage1._

class Vars1Util {
  var i = 0
  
  def createFreshVar() = {
    i += 1
    Variable1("$$" + i)
  }
  
  def createFreshLetrecVar() = {
    i += 1
    val freshLetrecVar = Variable1("$$" + i)
    freshLetrecVar.call = true
    freshLetrecVar
  }
}
