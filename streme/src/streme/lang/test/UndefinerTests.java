package streme.lang.test;

import junit.framework.TestCase;
import streme.lang.ast.AstDataCompiler;
import streme.lang.ast.Node;
import streme.lang.ast.analysis.Undefiner;
import streme.lang.ast.impl.StremeDataCompiler;
import streme.lang.data.DataUnifier;
import streme.lang.data.Parser2;

public class UndefinerTests extends TestCase
{
  private Parser2 parser = new Parser2();
  private AstDataCompiler compiler = new StremeDataCompiler();
  private Undefiner u = new Undefiner();
  private DataUnifier unifier = new DataUnifier();  

  private void undefine(String node, String expected)
  {
    Node ast = compiler.compile(parser.parse(node));
    Node result = u.rewrite(ast);
    assertTrue(unifier.matches(parser.parse(expected), result.toData()));
  }

  private void undefine(String node)
  {
    undefine(node, node);
  }

  public void testSimple()
  {
    undefine("(begin x)");
    undefine("(begin x y)");
    undefine("(begin x y z)");
  }
  
  public void testDefineValues()
  {
    undefine("(begin (define x 10) x)", "(let ((x ?)) (set! x 10) x)");
    undefine("(begin (define x 10) (define y 20) (+ x y))", "(let ((x ?)) (let ((y ?)) (set! x 10) (set! y 20) (+ x y)))");
    undefine("(begin 1 (define x 10) 3)", "(let ((x ?)) 1 (set! x 10) 3)");
    undefine("(begin 1 (define x 10) 3 (define y 20) 5)", "(let ((x ?)) (let ((y ?)) 1 (set! x 10) 3 (set! y 20) 5))");
  }
  
  public void testDefineLambdas()
  {
    undefine("(begin (define x (lambda () 10)) x)", "(let ((x ?)) (set! x (lambda () 10)) x)");
    undefine("(begin (define f (lambda (x) (g u))) (define u 123) (define g (lambda (y) (* y y))) (f 4))", "(let ((f ?)) (let ((u ?)) (let ((g ?)) (set! f (lambda (x) (g u))) (set! u 123) (set! g (lambda (y) (* y y))) (f 4))))");
  }  
}
