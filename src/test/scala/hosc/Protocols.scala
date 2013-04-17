package hosc

import hosc.{SuperCompilerApp => SCP0, SCLemApp}
import hosc.exp.{SuperCompilerApp1 => SCP1, SuperCompilerApp2 => SCP2}

object Protocols {
  def main(args : Array[String]) : Unit = {
    SCP1.main(Array("-si", "examples/protocols/01_synapse.hs",  "-t", "out/protocols/01_synapse.svg", "-p", "out/protocols/01_synapse.hs"))
    SCP0.main(Array("-si", "examples/protocols/02_msi.hs", "-t", "out/protocols/02_msi.svg", "-p", "out/protocols/02_msi.hs"))
    // slow
    //SCLemApp.main(Array("-si", "examples/protocols/03_mosi.hs", "-li", "examples/protocols/lemmas.hs", "-t", "out/protocols/03_mosi.svg", "-p", "out/protocols/03_mosi.hs"))
    // 20 sec
    SCP0.main(Array("-si", "examples/protocols/04_mesi.hs", "-t", "out/protocols/04_mesi.svg", "-p", "out/protocols/04_mesi.hs"))
    //SCP0.main(Array("-si", "examples/protocols/05_moesi.hs", "-t", "out/protocols/05_moesi.svg", "-p", "out/protocols/05_moesi.hs"))
    //SCP0.main(Array("-si", "examples/protocols/06_illinois.hs", "-t", "out/protocols/06_illinois.svg", "-p", "out/protocols/06_illinois.hs"))
    //SCLemApp.main(Array("-si", "examples/protocols/06_illinois.hs", "-li", "examples/protocols/lemmas.hs", "-t", "out/protocols/06_illinois.svg", "-p", "out/protocols/06_illinois.hs"))
    //SCP0.main(Array("-si", "examples/protocols/07_berkeley.hs", "-t", "out/protocols/07_berkeley.svg", "-p", "out/protocols/07_berkeley.hs"))
    //SCP0.main(Array("-si", "examples/protocols/08_dec.hs", "-t", "out/protocols/08_dec.svg", "-p", "out/protocols/08_dec.hs"))
    //SCLemApp.main(Array("-si", "examples/protocols/08_dec.hs", "-li", "examples/protocols/lemmas.hs", "-t", "out/protocols/08_dec.svg", "-p", "out/protocols/08_dec.hs"))
    //SCP0.main(Array("-si", "examples/protocols/09_futurebus.hs", "-t", "out/protocols/09_futurebus.svg", "-p", "out/protocols/09_futurebus.hs"))
    //SCLemApp.main(Array("-si", "examples/protocols/09_futurebus.hs", "-li", "examples/protocols/lemmas.hs", "-t", "out/protocols/09_futurebus.svg", "-p", "out/protocols/09_futurebus.hs"))
    //SCP0.main(Array("-si", "examples/protocols/10_xerox.hs", "-t", "out/protocols/10_xerox.svg", "-p", "out/protocols/10_xerox.hs"))
    //SCLemApp.main(Array("-si", "examples/protocols/10_xerox.hs", "-li", "examples/protocols/lemmas.hs", "-t", "out/protocols/10_xerox.svg", "-p", "out/protocols/10_xerox.hs"))
    //SCP0.main(Array("-si", "examples/protocols/11_java.hs", "-t", "out/protocols/11_java.svg", "-p", "out/protocols/11_java.hs"))
    //SCP0.main(Array("-si", "examples/protocols/12_reader_writer.hs", "-t", "out/protocols/12_reader_writer.svg", "-p", "out/protocols/12_reader_writer.hs"))
    //SCP0.main(Array("-si", "examples/protocols/13_german_i.hs", "-t", "out/protocols/13_german_i.svg", "-p", "out/protocols/13_german_i.hs"))
    //SCLemApp.main(Array("-si", "examples/protocols/13_german_i.hs", "-li", "examples/protocols/lemmas.hs", "-t", "out/protocols/13_german_i.svg", "-p", "out/protocols/13_german_i.hs"))
    //SCP0.main(Array("-si", "examples/protocols/14_german_b.hs", "-t", "out/protocols/14_german_b.svg", "-p", "out/protocols/14_german_b.hs"))
    //SCLemApp.main(Array("-si", "examples/protocols/14_german_b.hs", "-li", "examples/protocols/lemmas.hs", "-t", "out/protocols/14_german_b.svg", "-p", "out/protocols/14_german_b.hs"))
    //SCP0.main(Array("-si", "examples/protocols/15_data_race.hs", "-t", "out/protocols/15_data_race.svg", "-p", "out/protocols/15_data_race.hs"))
  }
}