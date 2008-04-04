package hosc;

import junit.framework.JUnit4TestAdapter;
import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests {

	public static Test suite() {
		TestSuite suite = new TestSuite("Tests for HOSC");
		//$JUnit-BEGIN$
		suite.addTest(new JUnit4TestAdapter(hosc.HParsersTest.class));
		suite.addTest(new JUnit4TestAdapter(hosc.TypeInfererTest.class));
		//$JUnit-END$
		return suite;
	}

}
