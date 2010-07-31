package hosc.re;

import junit.framework.JUnit4TestAdapter;
import junit.framework.Test;
import junit.framework.TestSuite;

public class RegularExpressions4J {
	public static Test suite() {
		TestSuite suite = new TestSuite("Regular Expressions");
		// $JUnit-BEGIN$
		suite.addTest(new JUnit4TestAdapter(hosc.re.RegularExpressions.class));
		// $JUnit-END$
		return suite;
	}
}
