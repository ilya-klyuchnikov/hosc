package hosc;

import junit.framework.JUnit4TestAdapter;
import junit.framework.Test;
import junit.framework.TestSuite;

public class PresentationExamples4J {

	public static Test suite() {
		TestSuite suite = new TestSuite("Presentation");
		// $JUnit-BEGIN$
		suite.addTest(new JUnit4TestAdapter(hosc.PresentationExamples.class));
		// $JUnit-END$
		return suite;
	}

}
