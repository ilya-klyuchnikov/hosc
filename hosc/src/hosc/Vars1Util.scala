package hosc;

import HLanguage1._

class Vars1Util {
  // counter for common variables
  var i = 0
  // counter for letrec variables
  var j = 0 
  
  def createFreshVar() = {
    i += 1
    Variable1("$$" + i)
  }
  
  def createFreshLetrecVar() = {
    j += 1
    val freshLetrecVar = Variable1("f$$" + j)
    freshLetrecVar.call = true
    freshLetrecVar
  }
}
