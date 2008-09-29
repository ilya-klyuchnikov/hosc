package hosc;

import junit.framework.JUnit4TestAdapter;
import junit.framework.Test;
import junit.framework.TestSuite;

public class TasksTestJ4 {

	public static Test suite() {
		TestSuite suite = new TestSuite("Tasks");
		// $JUnit-BEGIN$
		suite.addTest(new JUnit4TestAdapter(hosc.TasksTest.class));
		// $JUnit-END$
		return suite;
	}

}
