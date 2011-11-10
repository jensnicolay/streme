package streme.lang.eval.nd.test;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllNdTests
{
  public static Test suite()
  {
    TestSuite suite = new TestSuite("Nd");
    //$JUnit-BEGIN$
    suite.addTestSuite(NdAnalysisTransformationTests.class);
    //$JUnit-END$
    return suite;
  }
}