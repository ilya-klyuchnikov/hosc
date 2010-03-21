package hosc.tests;

import junit.framework.JUnit4TestAdapter;
import junit.framework.Test;
import junit.framework.TestSuite;

public class Equivalence4J {
	public static Test suite() {
		TestSuite suite = new TestSuite("Equivalence");
		// $JUnit-BEGIN$
		suite.addTest(new JUnit4TestAdapter(hosc.tests.Equivalence.class));
		// $JUnit-END$
		return suite;
	}
}
