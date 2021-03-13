package hosc

object TUtils {
  import hosc.HLanguage._
  import hosc.HParsers._
  import java.nio.file.{Files, Paths}
  import scala.util.parsing.input.CharSequenceReader

  def termResultFromString(input: String): ParseResult[Expression] =
    HParsers.parseTerm(new CharSequenceReader(input))

  def typeResultFromString(input: String): ParseResult[Type] =
    HParsers.parseType(new CharSequenceReader(input))

  def programResultFromFile(fileName: String): ParseResult[Program] = {
    val input = new String(Files.readAllBytes(Paths.get(fileName)))
    HParsers.parseProgram(new CharSequenceReader(input))
  }
}
