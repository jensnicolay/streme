package streme.lang.test;

import junit.framework.TestCase;
import streme.lang.ast.Node;
import streme.lang.ast.impl.SpPrinter;
import streme.lang.ast.impl.StremeSpDataCompiler2;
import streme.lang.data.SpParser2;

public class SpPrinterTests extends TestCase
{
  public void doTest(String source)
  {
    SpParser2 parser = new SpParser2(source);
    StremeSpDataCompiler2 compiler = new StremeSpDataCompiler2(parser.getSps());
    Node ast = compiler.compile(parser.next());
    String result = SpPrinter.print(ast);
    assertEquals(source, result);
  }
  
  public void testRef()
  {
    doTest("hello");
    doTest("   hello");
    doTest(" \t  \n  hello");
  }

  public void testConstant()
  {
    doTest("123");
    doTest("         123");
    doTest(" \n\t  123");
    doTest("12.3");
    doTest("         12.3");
    doTest(" \n\t  12.3");
    doTest("\"123\"");
    doTest("         \"123\"");
    doTest(" \n\t  \"123\"");
    doTest("#t");
    doTest("      #t");
    doTest(" \n\t  #t");
    doTest("#f");
    doTest("      #f");
    doTest(" \n\t  #f");
  }
  
  public void testDatum()
  {
    doTest("'abc");
    doTest("         'abc");
    doTest(" \n\t  'abc");
  }
  
  public void testList()
  {
    doTest("()");
    doTest("      ()");
    doTest("   \n  \t ()");
    doTest("(f a b)");
    doTest("      (f a b)");
    doTest("   \n  \t (f a b)");
    doTest("   \n  \t (f   a     b)");
    doTest("   \n  \t (f a   \n  \tb)");
    doTest("   \n  \t (f a\n\t\t\t\tb)");
    doTest("\n\t(\nf\na\tb)");
    doTest("\n\t(\nf\n1\t2)");
    doTest("  (  1 a 'b \"2\"   )");
    doTest("(f a b #t)");
  }


  public void testDatumLists()
  {
    doTest("'(1 2 3)");
    doTest("         '(1 2 3)");
    doTest(" \n\t  '(1 2 3)");
    doTest("'(1 2 3)");
    doTest("         '(1  2        3)");
    doTest(" \n\t  '(1\n 2\t    3)");
    doTest("   '(       1\n   2\t    3   )");
  }
  
  public void testDefine()
  {
    doTest("(define a 1)");
    doTest("   (define   a        1)");
    doTest("        \n(define\n  a\t  1)");
    doTest("        \n(        define\n  a\t  1  )");
  }
  
  public void testSetVar()
  {
    doTest("(set! a 1)");
    doTest("   (set!   a        1)");
    doTest("        \n(dset!\n  a\t  1)");
    doTest("        \n(        set!\n  a\t  1  )");
  }
  
  public void testBegin()
  {
    doTest("(begin a 1)");
    doTest("   (begin   a        1)");
    doTest("        \n(begin\n  a\t  1)");
    doTest("        \n(        begin\n  a\t  1  )");
  }
  
  public void testIf()
  {
    doTest("(if a 1 2)");
    doTest("   (if   a        1    2)");
    doTest("        \n(if\n  a\t  1       2)");
    doTest("        \n(        if\n  a\t  1        2    )");
  }
  
  public void testLambda()
  {
    doTest("(lambda (x y) (+ x y))");
    doTest("    (    lambda    (  x   y    )   (  +    x     y   )   )");
    doTest("\t(lambda (  x  y )\n(+ x y)\n(* x y)   )");
  }
  
  public void testLet()
  {
    doTest("(let ((a 1) (b 1)) c)");
    doTest("     (let ((\t\t\ta\n1      )\n\t( b   1)  )    c d   e  )");
  }
  
  public void testGeneral()
  {
    doTest("(define writez\n    (lambda ()\n        (set! z 123)))");
  }
}
