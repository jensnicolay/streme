package streme.lang.test;

import java.util.concurrent.ExecutorService;

import jsr166y.ForkJoinPool;
import junit.framework.Assert;
import junit.framework.TestCase;
import streme.lang.eval.tanfe.TanfStreme;

public class StremeTestCase extends TestCase
{
  protected static final String UNDEFINED = "<undefined>";
  private TanfStreme streme1;
  private TanfStreme streme2;
  private TanfStreme streme3;
  private ExecutorService executor;
  
  public final void setUp()
  {
//    streme1 = new Streme();
//    streme1.addDataRewriter(new MacroExpander());
//
//    streme2 = new Streme();
//    MacroExpander me2 = new MacroExpander();
//    me2.setLetToLambda(false);
//    me2.setLetrecToLambda(false);
//    streme2.addDataRewriter(me2);
//    
//    streme3 = new Streme();
//    streme3.addDataRewriter(new MacroExpander());
//    streme3.addAstRewriter(new AnfConverter(RenamingStrategy.NUMBER_RENAMING_STRATEGY));
    
    executor = new ForkJoinPool();
    streme1 = new TanfStreme(executor);

//    streme2 = new Streme3(anfe);
//    MacroExpander me2 = new MacroExpander();
//    me2.setLetToLambda(false);
//    me2.setLetrecToLambda(false);
//    streme2.addDataRewriter(me2);
//    streme2.addAstRewriter(new AlphaConverter(RenamingStrategy.NUMBER_RENAMING_STRATEGY));
//    streme2.addAstRewriter(new AnfConverter(RenamingStrategy.NUMBER_RENAMING_STRATEGY));
  }
  
  protected void tearDown() throws Exception
  {
    executor.shutdown();
    super.tearDown();
  }
  
  protected void test(String expected, String... in)
  {
    int i;
    for (i = 0; i < in.length - 1; i++)
    {
     streme1.readEvalPrint(in[i]); 
    }
    Assert.assertEquals(expected, streme1.readEvalPrint(in[i]));
//    Assert.assertEquals(expected, streme2.readEvalPrint(in));
  }
}
