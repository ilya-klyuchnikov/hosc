package hosc.spec;

import junit.framework.JUnit4TestAdapter;
import junit.framework.Test;
import junit.framework.TestSuite;

public class SuperCompilerSpec4J {
	public static Test suite() {
		TestSuite suite = new TestSuite("SuperCompilerSpec");
		// $JUnit-BEGIN$
		suite.addTest(new JUnit4TestAdapter(hosc.spec.SuperCompilerSpec.class));
		// $JUnit-END$
		return suite;
	}
}
