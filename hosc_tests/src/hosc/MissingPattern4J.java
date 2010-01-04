package hosc;

import junit.framework.JUnit4TestAdapter;
import junit.framework.Test;
import junit.framework.TestSuite;

public class MissingPattern4J {

	public static Test suite() {
		TestSuite suite = new TestSuite("MissingPattern");
		// $JUnit-BEGIN$
		suite.addTest(new JUnit4TestAdapter(hosc.MissingPattern.class));
		// $JUnit-END$
		return suite;
	}

}
