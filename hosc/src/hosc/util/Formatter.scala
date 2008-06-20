package hosc.util;

import hosc.HLanguage1._

object Formatter {
  def format(term: Term1): String = {
    val writer = new java.io.StringWriter()
    term.toDoc.format(120, writer)
    writer.toString
  }
}
