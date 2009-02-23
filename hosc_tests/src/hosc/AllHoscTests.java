package hosc;

import junit.framework.JUnit4TestAdapter;
import junit.framework.Test;
import junit.framework.TestSuite;

public class AllHoscTests {

	public static Test suite() {
		TestSuite suite = new TestSuite("Tests for HOSC");
		// $JUnit-BEGIN$
		suite.addTest(new JUnit4TestAdapter(hosc.HParsersTest.class));
		suite.addTest(new JUnit4TestAdapter(hosc.TypeInferrerTest.class));
		suite.addTest(new JUnit4TestAdapter(hosc.GraphAnalysisTest.class));
		suite.addTest(new JUnit4TestAdapter(hosc.InterpreterTest.class));
		//suite.addTest(new JUnit4TestAdapter(hosc.MSGTest.class));
		suite.addTest(new JUnit4TestAdapter(hosc.TermAlgebraTest.class));
		suite.addTest(new JUnit4TestAdapter(hosc.HE1Test.class));
		suite.addTest(new JUnit4TestAdapter(hosc.Driver1Test.class));
		suite.addTest(new JUnit4TestAdapter(hosc.SuperCompilerTest.class));
		suite.addTest(new JUnit4TestAdapter(hosc.SuperCompiler0Test.class));
		suite.addTest(new JUnit4TestAdapter(hosc.Supercompiler1Test.class));
		//suite.addTest(new JUnit4TestAdapter(hosc.SuperCompiler2Test.class));
		suite.addTest(new JUnit4TestAdapter(hosc.TasksTest.class));
		// $JUnit-END$
		return suite;
	}

}
