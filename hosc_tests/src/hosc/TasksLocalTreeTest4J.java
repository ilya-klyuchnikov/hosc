package hosc;

import junit.framework.JUnit4TestAdapter;
import junit.framework.Test;
import junit.framework.TestSuite;

public class TasksLocalTreeTest4J {

	public static Test suite() {
		TestSuite suite = new TestSuite("Tasks");
		// $JUnit-BEGIN$
		suite.addTest(new JUnit4TestAdapter(hosc.TasksLocalTreeTest.class));
		// $JUnit-END$
		return suite;
	}

}
