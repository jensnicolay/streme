package streme.lang.ast.test;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllAstTests
{
  public static Test suite()
  {
    TestSuite suite = new TestSuite("AST");
    //$JUnit-BEGIN$
    suite.addTestSuite(AnfConverterTests.class);
    suite.addTestSuite(AlphaConverterTests.class);
    suite.addTestSuite(NodeUnifierTests.class);
    suite.addTestSuite(SyntaxSimplifierTests.class);
    //$JUnit-END$
    return suite;
  }
}