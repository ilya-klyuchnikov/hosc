package hosc

import scala.util.parsing.input.CharArrayReader

import org.junit.Test
import org.junit.Assert._
import HLanguage._

object TestUtils {
  def termResultFromString(input: String) =
    HParsers.parseTerm(new CharArrayReader(input.toCharArray))
}
