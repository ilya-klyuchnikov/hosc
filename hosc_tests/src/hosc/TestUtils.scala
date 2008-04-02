package hosc

import scala.util.parsing.input.{CharArrayReader, Reader}
import java.io.{BufferedReader, File, FileReader}

import org.junit.Test
import org.junit.Assert._
import HLanguage._

object TestUtils {
  def termResultFromString(input: String) =
    HParsers.parseTerm(new CharArrayReader(input.toCharArray))
    
  def programResultFromString(input: String) =
    HParsers.parseProgram(new CharArrayReader(input.toCharArray))
    
  def programResultFromFile(fileName: String) = {
    val file = new File(fileName)
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
