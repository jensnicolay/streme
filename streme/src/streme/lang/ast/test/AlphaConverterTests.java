package streme.lang.ast.test;

import junit.framework.TestCase;
import streme.lang.ast.AstDataCompiler;
import streme.lang.ast.analysis.RenamingStrategy;
import streme.lang.ast.impl.AlphaConverter;
import streme.lang.ast.impl.StremeDataCompiler;
import streme.lang.data.Data;
import streme.lang.data.Parser2;
import streme.lang.data.Sym;


public class AlphaConverterTests extends TestCase
{
  private Parser2 parser = new Parser2();
  private AstDataCompiler compiler = new StremeDataCompiler();
  
  public void test(String in, String expected)
  {
    AlphaConverter renamer = new AlphaConverter(new RenamingStrategy()
    {
      private int counter;
      
      public Sym rename(Sym original)
      {
        return new Sym(original.toString() + counter++);
      }
    });
    assertEquals(expected, Data.toString(renamer.rewrite(compiler.compile(parser.parse(in)))));
  }
  
  public void testLambda()
  {
    test("(define x 1)", "(define x 1)");
    test("(lambda () (+ x y))", "(lambda () (+ x y))");
    test("(lambda (x) (+ x y))", "(lambda (x0) (+ x0 y))");
    test("(lambda (x y) (+ x y))", "(lambda (x0 y1) (+ x0 y1))");
    test("(lambda (x) (+ x y) (* y x))", "(lambda (x0) (+ x0 y) (* y x0))");
    test("(lambda (x) (+ x y) ((lambda (x) (* x y)) x))", "(lambda (x0) (+ x0 y) ((lambda (x1) (* x1 y)) x0))");
    test("(lambda (x) '(lambda) (+ lambda x))", "(lambda (x0) (lambda) (+ lambda x0))");
    test("(lambda (x) (lambda (x) x))", "(lambda (x0) (lambda (x1) x1))");
  }

  public void testLet()
  {
    test("(let ((x 1)) x)", "(let ((x0 1)) x0)");
    test("(let ((x 1)) (let ((x 2)) x) x)", "(let ((x0 1)) (let ((x1 2)) x1) x0)");
    test("(let ((x x)) (let ((x x)) x) x)", "(let ((x0 x)) (let ((x1 x0)) x1) x0)");
  }

  public void testLetRec()
  {
    test("(letrec ((x 1)) x)", "(letrec ((x0 1)) x0)");
    test("(letrec ((f (lambda () f))) f)", "(letrec ((f0 (lambda () f0))) f0)");
    test("(letrec ((f (lambda () f)) (g (lambda () f))) g)", "(letrec ((f0 (lambda () f0)) (g1 (lambda () f0))) g1)");
  }
  
  public void testLetRec2()
  {
    test("(letrec ((f (lambda () (g))) (g (lambda () 123))) (f) (g))", "(letrec ((f0 (lambda () (g1))) (g1 (lambda () 123))) (f0) (g1))"); 
  }
  
  public void testSet()
  {
    test("(let ((x '())) (set! x (cons 123 x)))", "(let ((x0 ())) (set! x0 (cons 123 x0)))");
  }

}
