package hosc;

import junit.framework.JUnit4TestAdapter;
import junit.framework.Test;
import junit.framework.TestSuite;

public class TypeInferrerTest4J {
	public static Test suite() {
		TestSuite suite = new TestSuite("Type Inferrer");
		// $JUnit-BEGIN$
		suite.addTest(new JUnit4TestAdapter(hosc.TypeInferrerTest.class));
		// $JUnit-END$
		return suite;
	}
}
