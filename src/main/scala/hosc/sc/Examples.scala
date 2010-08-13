package hosc.sc

object Examples {
  val supercompilers: List[SuperCompilerFacade] = List(
    HOSC15,
    NaiveSuperCompiler,
    NaiveSuperCompilerWithCoupling,
    NaiveSuperCompilerWithControl,
    NaiveSuperCompilerWithControlAndCoupling,
    NaiveSuperCompilerTransientAware)

  def run(file: String): Unit = {
    val in = "examples/" + file
    println()
    println()
    println("===RUN:===")
    println(file)
    for (sc <- supercompilers) {
    	try {
    		println()
    		println("***" + sc.name + "***")
    		val resProg = sc.superCompileFile(in)
    		println(resProg.toDocString)
    	} catch {
    		case e => println(e.getMessage)
    	}
    }
  }
}