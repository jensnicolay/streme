package streme.lang.test;

import junit.framework.TestCase;
import streme.lang.ast.AstDataCompiler;
import streme.lang.ast.Node;
import streme.lang.ast.impl.StremeDataCompiler;
import streme.lang.ast.impl.SyntaxSimplifier;
import streme.lang.data.Parser2;

public class SyntaxSimplifierTests extends TestCase
{
  private Parser2 parser = new Parser2();
  private AstDataCompiler compiler = new StremeDataCompiler();

  private Node createAst(String node)
  {
    return compiler.compile(parser.parse(node));
  }

  public void testBegin1()
  {
    String source = "(begin x)";
    Node ast = createAst(source);
    assertEquals("x", new SyntaxSimplifier().rewrite(ast).toString());
  }
  
  public void testBegin2()
  {
    String source = "(begin x y)";
    Node ast = createAst(source);
    assertEquals(source, new SyntaxSimplifier().rewrite(ast).toString());
  }
  
  public void testLet1()
  {
    String source = "(let ((x1 v1)) x1)";
    Node ast = createAst(source);
    assertEquals("v1", new SyntaxSimplifier().rewrite(ast).toString());
  }
  
  public void testLet2()
  {
    String source = "(let ((x1 v1) (x2 v2)) x2)";
    Node ast = createAst(source);
    assertEquals("(let ((x1 v1)) v2)", new SyntaxSimplifier().rewrite(ast).toString());
  }
  
  public void testLet3()
  {
    String source = "(let ((x1 v1) (x2 v2)) x1)";
    Node ast = createAst(source);
    assertEquals(source, new SyntaxSimplifier().rewrite(ast).toString());
  }
  
  
}
