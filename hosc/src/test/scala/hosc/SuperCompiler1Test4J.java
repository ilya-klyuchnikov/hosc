package hosc;

import junit.framework.JUnit4TestAdapter;
import junit.framework.Test;
import junit.framework.TestSuite;

public class SuperCompiler1Test4J {

	public static Test suite() {
		TestSuite suite = new TestSuite("SuperCompiler0");
		// $JUnit-BEGIN$
		suite.addTest(new JUnit4TestAdapter(hosc.SuperCompiler1Test.class));
		// $JUnit-END$
		return suite;
	}

}