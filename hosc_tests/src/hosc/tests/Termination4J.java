package hosc.tests;

import junit.framework.JUnit4TestAdapter;
import junit.framework.Test;
import junit.framework.TestSuite;

public class Termination4J {
	public static Test suite() {
		TestSuite suite = new TestSuite("Termination");
		// $JUnit-BEGIN$
		suite.addTest(new JUnit4TestAdapter(hosc.tests.Termination.class));
		// $JUnit-END$
		return suite;
	}
}
