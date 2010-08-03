package hosc.spec.hl

import hosc.exp.FastHLSCApp

object FastHLSCESpec {
  def main(args: Array[String]): Unit = {
    FastHLSCApp.main(Array("-si", "spec_fhl/doublea.hs", "-t", "spec_fhl/dump/doublea.svg", "-p", "spec_fhl/dump/doublea.hs"));
  }
}