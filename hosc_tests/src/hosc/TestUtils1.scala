package hosc

import scala.util.parsing.input.{CharArrayReader, Reader}
import java.io.{BufferedReader, File, FileReader}
import sc1.HParsers1
import sc1.Postprocessor1._

import org.junit.Test
import org.junit.Assert._

object TestUtils1 {
  def termResultFromString(input: String) =
    HParsers1.parseTerm(new CharArrayReader(input.toCharArray))
    
  def termFromString(input: String) ={ 
    val r = termResultFromString(input)
    println(r)
    val res = r.get
    postprocess(res)
    res
  }
    
  def termFromFile(fileName: String) = {
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
      val r = HParsers1.parseTerm(new CharArrayReader(sb.toString.toCharArray))
      println(r)
      val res = r.get
      postprocess(res)
      res
  }
}
