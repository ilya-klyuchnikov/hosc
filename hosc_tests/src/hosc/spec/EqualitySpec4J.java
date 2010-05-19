package hosc.spec;

import junit.framework.JUnit4TestAdapter;
import junit.framework.Test;
import junit.framework.TestSuite;

public class EqualitySpec4J {
	public static Test suite() {
		TestSuite suite = new TestSuite("EqualitySpec");
		// $JUnit-BEGIN$
		suite.addTest(new JUnit4TestAdapter(hosc.spec.EqualitySpec.class));
		// $JUnit-END$
		return suite;
	}
}
