package hosc

import java.io.{BufferedReader, FileReader}

import hosc.HLanguage._

import scala.util.parsing.input.CharArrayReader

object TUtils {
  def termResultFromString(input: String): HParsers.ParseResult[Expression] =
    HParsers.parseTerm(new CharArrayReader(input.toCharArray))

  def typeExprResultFromString(input: String): HParsers.ParseResult[Type] =
    HParsers.parseType(new CharArrayReader(input.toCharArray))

  def programResultFromFile(fileName: String): HParsers.ParseResult[Program] = {
    val sb = new StringBuilder
    val in = new BufferedReader(new FileReader(fileName));
    var str: String = null
    do {
      str = in.readLine
      if (str != null){
        sb.append(str)
        sb.append("\n")
      }
    } while (str != null)
    in.close();
    HParsers.parseProgram(new CharArrayReader(sb.toString.toCharArray))
  }
}
