package hosc

import org.junit.Test
import org.junit.Ignore
import org.junit.Assert._
import hosc.{ SuperCompilerApp => SCP0 }

class MissingPattern {
  val examplesDir = "examples/"
  @Test
  def mp1a =
    SCP0.main(Array("-si", examplesDir + "mp/mp1a.hs", "-t", examplesDir + "mp/out0/mp1a.svg", "-p", examplesDir + "mp/out0/mp1a.hs"))

  @Test
  def mp1b =
    SCP0.main(Array("-si", examplesDir + "mp/mp1b.hs", "-t", examplesDir + "mp/out0/mp1b.svg", "-p", examplesDir + "mp/out0/mp1b.hs"))

  @Test
  def empty =
    SCP0.main(Array("-si", examplesDir + "mp/empty.hs", "-t", examplesDir + "mp/out0/empty.svg", "-p", examplesDir + "mp/out0/empty.hs"))
}
