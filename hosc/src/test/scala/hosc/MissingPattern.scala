package hosc

import org.junit.Test
import org.junit.Ignore
import org.junit.Assert._
import hosc.{SuperCompilerApp => SCP0}

class MissingPattern {
  @Test def mp1a =
    SCP0.main(Array("-si", "mp/mp1a.hs", "-t", "mp/out0/mp1a.svg","-p", "mp/out0/mp1a.hs"))
  
  @Test def mp1b =
    SCP0.main(Array("-si", "mp/mp1b.hs", "-t", "mp/out0/mp1b.svg","-p", "mp/out0/mp1b.hs"))
  
  @Test def empty =
    SCP0.main(Array("-si", "mp/empty.hs", "-t", "mp/out0/empty.svg","-p", "mp/out0/empty.hs"))
}
