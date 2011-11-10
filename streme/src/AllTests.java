

import junit.framework.Test;
import junit.framework.TestSuite;
import streme.lang.ast.test.AlphaConverterTests;
import streme.lang.ast.test.AnfConverterTests;
import streme.lang.data.test.CircularTests;
import streme.lang.eval.nd.test.AllNdTests;
import streme.lang.test.BruteForceParallelizerTests;
import streme.lang.test.DataTrsTests;
import streme.lang.test.DataUnifierTests;
import streme.lang.test.DependenceAnalyzerTests;
import streme.lang.test.IpdAnalyzerTests;
import streme.lang.test.IpdaTests;
import streme.lang.test.KcfaTests;
import streme.lang.test.LexicalSimplifiersTests;
import streme.lang.test.MacroExpanderTests;
import streme.lang.test.MiscTests;
import streme.lang.test.NodeUnifierTests;
import streme.lang.test.R5RSTests;
import streme.lang.test.SpDataCompilerTests;
import streme.lang.test.SpParserTests;
import streme.lang.test.SpPrinterTests;
import streme.lang.test.StremeTests;
import streme.lang.test.SyntaxSimplifierTests;
import streme.lang.test.UndefinerTests;
import streme.lang.test.VarPointerTests;

public class AllTests
{
  public static Test suite()
  {
    TestSuite suite = new TestSuite("Tests for Streme");
    //$JUnit-BEGIN$
    suite.addTestSuite(MacroExpanderTests.class);
    suite.addTestSuite(AlphaConverterTests.class);
    suite.addTestSuite(AnfConverterTests.class);
    suite.addTestSuite(SyntaxSimplifierTests.class);
    suite.addTestSuite(LexicalSimplifiersTests.class);
    suite.addTestSuite(StremeTests.class);
    suite.addTestSuite(DataUnifierTests.class);
    suite.addTestSuite(NodeUnifierTests.class);
    suite.addTestSuite(MiscTests.class);
    suite.addTestSuite(IpdaTests.class);
    suite.addTestSuite(CircularTests.class);
    suite.addTestSuite(R5RSTests.class);
    suite.addTestSuite(KcfaTests.class);
    suite.addTestSuite(DataTrsTests.class);
    suite.addTestSuite(BruteForceParallelizerTests.class);
    suite.addTestSuite(UndefinerTests.class);
    suite.addTestSuite(VarPointerTests.class);
    suite.addTestSuite(IpdAnalyzerTests.class);
    suite.addTestSuite(DependenceAnalyzerTests.class);
    suite.addTestSuite(SpParserTests.class);
    suite.addTestSuite(SpDataCompilerTests.class);
    suite.addTestSuite(SpPrinterTests.class);
    suite.addTest(AllNdTests.suite());
    //$JUnit-END$
    return suite;
  }
}
