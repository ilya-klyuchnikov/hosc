package hosc.util;

import hosc.HLanguage1._
import hosc.HLanguage._

object Formatter {
  def format(term: Expression1): String = {
    val writer = new java.io.StringWriter()
    term.toDoc.format(120, writer)
    writer.toString
  }
  
  def format(expr: Expression): String = {
    val writer = new java.io.StringWriter()
    expr.toDoc.format(120, writer)
    writer.toString
  }
}
