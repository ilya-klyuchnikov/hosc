package hosc;

import junit.framework.JUnit4TestAdapter;
import junit.framework.Test;
import junit.framework.TestSuite;

public class SmartEmbeddingTest4J {
	public static Test suite() {
		TestSuite suite = new TestSuite("SmartEmbedding");
		// $JUnit-BEGIN$
		suite.addTest(new JUnit4TestAdapter(hosc.SmartEmbeddingTest.class));
		// $JUnit-END$
		return suite;
	}
}
