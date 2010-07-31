package hosc;

import junit.framework.JUnit4TestAdapter;
import junit.framework.Test;
import junit.framework.TestSuite;

public class EqTest4J {
	public static Test suite() {
		TestSuite suite = new TestSuite("Eq");
		// $JUnit-BEGIN$
		suite.addTest(new JUnit4TestAdapter(hosc.EqTest.class));
		// $JUnit-END$
		return suite;
	}
}
