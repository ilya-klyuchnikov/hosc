package hosc.spec.hl

import hosc.exp.FastHLSCApp

// example of superlinear speed-up by means of higher-level supercompilation
object FastHLSCESpec {
  def main(args: Array[String]): Unit = {
    FastHLSCApp.main(Array("-si", "examples/spec_fhl/doublea.hs", "-t", "out/spec_fhl/dump/doublea.svg", "-p", "out/spec_fhl/dump/doublea.hs"));
  }
}