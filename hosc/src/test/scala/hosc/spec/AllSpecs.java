package hosc.spec;

import junit.framework.JUnit4TestAdapter;
import junit.framework.Test;
import junit.framework.TestSuite;

public class AllSpecs {

	public static Test suite() {
		TestSuite suite = new TestSuite("Specs for HOSC");
		// $JUnit-BEGIN$
		suite.addTest(new JUnit4TestAdapter(hosc.spec.SuperCompilerSpec.class));
		suite.addTest(new JUnit4TestAdapter(hosc.spec.EqualitySpec.class));
		suite.addTest(new JUnit4TestAdapter(hosc.spec.HigherLevelSuperCompilerSpec.class));
		// $JUnit-END$
		return suite;
	}

}
