package hosc.spec;

import junit.framework.JUnit4TestAdapter;
import junit.framework.Test;
import junit.framework.TestSuite;

public class HigherLevelSuperCompilerSpec4J {
	public static Test suite() {
		TestSuite suite = new TestSuite("HigherLevelSuperCompilerSpec");
		// $JUnit-BEGIN$
		suite.addTest(new JUnit4TestAdapter(hosc.spec.HigherLevelSuperCompilerSpec.class));
		// $JUnit-END$
		return suite;
	}
}
