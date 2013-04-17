package hosc
import hosc.exp.{FullPositiveSuperCompilerApp => FPSC}
import hosc.{SuperCompilerApp => SC}
object FullPositiveExamples {
  def main(args : Array[String]) : Unit = {
    FPSC.main(Array("-si", "examples/re/regexp2.hs", "-t", "out/re/regexp2.svg","-p", "out/re/regexp2.hs"))
    //FPSC.main(Array("-si", "hl/church.hs", "-t", "hl/out/church.svg","-p", "hl/out/church.hs"))
  }

}
