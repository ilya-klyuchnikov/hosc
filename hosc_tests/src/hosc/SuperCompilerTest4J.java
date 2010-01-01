package hosc;

import junit.framework.JUnit4TestAdapter;
import junit.framework.Test;
import junit.framework.TestSuite;

public class SuperCompilerTest4J {

	public static Test suite() {
		TestSuite suite = new TestSuite("SuperCompiler");
		// $JUnit-BEGIN$
		suite.addTest(new JUnit4TestAdapter(hosc.SuperCompilerTest.class));
		// $JUnit-END$
		return suite;
	}

}
