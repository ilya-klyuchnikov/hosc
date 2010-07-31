package hosc;

import junit.framework.JUnit4TestAdapter;
import junit.framework.Test;
import junit.framework.TestSuite;

public class LambdaLiftingTest4J {
	public static Test suite() {
		TestSuite suite = new TestSuite("LambdaLifting");
		// $JUnit-BEGIN$
		suite.addTest(new JUnit4TestAdapter(hosc.LambdaLiftingTest.class));
		// $JUnit-END$
		return suite;
	}
}
