package streme.lang.analysis.test;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllAnalysisTests
{
  public static Test suite()
  {
    TestSuite suite = new TestSuite("Analysis");
    //$JUnit-BEGIN$
    suite.addTestSuite(VarPointerTests.class);
    suite.addTestSuite(DependenceAnalyzerTests.class);
    suite.addTestSuite(IpdAnalyzerTests.class);
    suite.addTestSuite(IpdaTests.class);
    suite.addTestSuite(KcfaTests.class);
    //$JUnit-END$
    return suite;
  }
}