package hosc;

import junit.framework.JUnit4TestAdapter;
import junit.framework.Test;
import junit.framework.TestSuite;

public class Experiments4J {

	public static Test suite() {
		TestSuite suite = new TestSuite("Experiments");
		// $JUnit-BEGIN$
		suite.addTest(new JUnit4TestAdapter(hosc.Experiments.class));
		// $JUnit-END$
		return suite;
	}

}
