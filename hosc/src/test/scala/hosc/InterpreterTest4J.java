package hosc;

import junit.framework.JUnit4TestAdapter;
import junit.framework.Test;
import junit.framework.TestSuite;

public class InterpreterTest4J {
	public static Test suite() {
		TestSuite suite = new TestSuite("Interpreter");
		// $JUnit-BEGIN$
		suite.addTest(new JUnit4TestAdapter(hosc.InterpreterTest.class));
		// $JUnit-END$
		return suite;
	}
}
