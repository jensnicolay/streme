package streme.lang.test;

import junit.framework.TestCase;
import streme.lang.ast.AstVisitor;
import streme.lang.ast.Node;
import streme.lang.ast.impl.StremeSpDataCompiler2;
import streme.lang.data.SpData;
import streme.lang.data.SpParser2;

public class SpDataCompilerTests extends TestCase
{
  public void testo()
  {
    doSanityChecks("(*  'hiphoi)", "(* hiphoi)");
    doSanityChecks("(define   a \n  \t1)", "(define a 1)");
    doSanityChecks("(lambda\t()\n42)", "(lambda () 42)");
    doSanityChecks("(lambda\t( x )\n42)  ", "(lambda (x) 42)");
    doSanityChecks("(lambda (  x    )     (*  #(  1   2)   'hiphoi))", "(lambda (x) (* #(1 2) hiphoi))");
    doSanityChecks("(lambda (x) 1 2 3)");
    doSanityChecks("   (if \n\n\t  'cond \n  #f    23)", "(if cond #f 23)");
    doSanityChecks("(define a (lambda (x) (* x x)))");
    doSanityChecks("( let  \n  (  (a \t1 ) ) \t a)", "(let ((a 1)) a)");
    doSanityChecks("(begin (lambda (x) y) 34.535 34 \"hey\")");
    doSanityChecks("(future (lambda (x) y 34.535 34 '#(hop)))", "(future (lambda (x) y 34.535 34 #(hop)))");
    doSanityChecks("(set! x 1)");
    doSanityChecks("(quote helaba)  ", "helaba");
    doSanityChecks("         '(1   2         3)    ", "(1 2 3)");
  }

  public void doSanityChecks(String source)
  {
    doSanityChecks(source, source);
  }

  private void doSanityChecks(String source, String normalized)
  {
    SpParser2 parser = new SpParser2(source);
    Object data = parser.next();
    StremeSpDataCompiler2 compiler = new StremeSpDataCompiler2(parser.getSps());
    Node node = compiler.compile(data);
    assertEquals(normalized, node.toString());
    verifySpAnnotationsPresent(node);
    SpData sp = (SpData) node.getProperty("sp");
    assertEquals(source.trim(), source.substring(sp.getPos(), sp.getEndPos()));
  }

  private void verifySpAnnotationsPresent(Node node)
  {
    node.accept(new AstVisitor()
    {
      public boolean visitNode(Node node)
      {
        SpData sp = (SpData) node.getProperty("sp");
        assertNotNull(sp);
        assertNotNull(sp.getPrefix());
        return true;
      }
    });
  }
}
