package streme.lang.ast.test;

import junit.framework.TestCase;
import streme.lang.ast.AstDataCompiler;
import streme.lang.ast.Node;
import streme.lang.ast.analysis.AnfConverter;
import streme.lang.ast.analysis.RenamingStrategy;
import streme.lang.ast.impl.StremeDataCompiler;
import streme.lang.data.DataUnifier;
import streme.lang.data.Parser2;

public class AnfConverterTests extends TestCase
{
  private Parser2 parser = new Parser2();
  private DataUnifier unifier = new DataUnifier();
  private AstDataCompiler compiler = new StremeDataCompiler();
  
  private void convert(String source, String expected)
  {
    AnfConverter anfConverter = new AnfConverter(RenamingStrategy.NUMBER_RENAMING_STRATEGY);
    Node anf = anfConverter.rewrite(compiler.compile(parser.parse(source)));
    assertTrue(anf.toData().toString(), unifier.matches(parser.parse(expected), anf.toData()));
  }


  public void test1()
  {
    convert("(f 1)", "(f 1)");
    convert("(f (g 2))", "(let ((?p0 (g 2))) (f ?p0))");
    convert("(f (g (h 1)) (i 2))", "(let ((?p1 (h 1))) (let ((?p2 (g ?p1))) (let ((?p3 (i 2))) (f ?p2 ?p3))))");
    convert("(let ((a 1)) (h a))", "(let ((a 1)) (h a))");
    convert("((lambda (p) 'body) 1)", "(let ((p 1)) body)");
    convert("((lambda (p q) 'body) 1 2)", "(let ((p 1)) (let ((q 2)) body))");
    convert("(f 1 (+ 2 3) 4)", "(let ((?p4 (+ 2 3))) (f 1 ?p4 4))");
    convert("(define f (lambda (n) (* n (g 1))))", "(define f (lambda (n) (let ((?p5 (g 1))) (* n ?p5))))");
    convert("(define fact (lambda (n) (if (zero? n) 1 (* n (fact (- n 1))))))", "(define fact (lambda (n) (let ((?p6 (zero? n))) (if ?p6 1 (let ((?p7 (- n 1))) (let ((?p8 (fact ?p7))) (* n ?p8)))))))");
    convert("(let ((a 2) (b (+ a 5))) (* a b))", "(let ((a 2)) (let ((b (+ a 5))) (* a b)))");
    convert("((lambda (a b) (* a b)) 2 (+ a 5))", "(let ((?p10 (+ a 5))) (let ((a 2)) (let ((b ?p10)) (* a b))))");
    convert("(let ((z 0)) (let ((writez (lambda () (set! z (+ z 1))))) (writez)))", "(let ((z 0)) (let ((writez (lambda () (let ((?p (+ z 1))) (set! z ?p))))) (writez)))");
    convert("(let ((z 0)) (let ((writez (lambda () (set! z 123))) (readz (lambda () z))) (let ((a (readz)) (b (writez))) (h a b))))",
    "(let ((z 0)) (let ((writez (lambda () (set! z 123)))) (let ((readz (lambda () z))) (let ((a (readz))) (let ((b (writez))) (h a b))))))");
    convert("(let () (+ a (* b c)))", "(let ((?p (* b c))) (+ a ?p))");
    convert("(let* ((a 1) (b a)) (+ a b))", "(let ((a 1)) (let ((b a)) (+ a b)))");
  }
  
  public void testBeginWithoutLetTailCall()
  {
    convert("(begin e1 e2)", "(let ((?p0 e1)) e2)");
    convert("(begin e1 e2 e3)", "(let ((?p0 e1)) (let ((?p1 e2)) e3))");
    convert("(begin (e1) (e2))", "(let ((?p0 (e1))) (e2))");
    convert("(begin (e1 (+ v0 v1)) (e2 v2))", "(let ((?p0 (+ v0 v1))) (let ((?p1 (e1 ?p0))) (e2 v2)))");
    convert("(begin e1 (+ e2 (* e3 e4)))", "(let ((?p0 e1)) (let ((?p1 (* e3 e4))) (+ e2 ?p1)))");
  }
  
  public void testVararg()
  {
    convert("(lambda (a . b) #t)", "(lambda (a . b) #t)");
  }
  
  public void testDefine()
  {
    convert("(define a (+ b c))", "(let ((?p1 (+ b c))) (define a ?p1))");
    convert("(define counter (let ((c 0)) (lambda () (set! c (+ c 1)) c)))", "(let ((c 0)) (define counter (lambda () (let ((?p0 (+ c 1))) (let ((?p1 (set! c ?p0))) c)))))");
    convert("(define f (future (fib 30)))", "(let ((?p1 (future (fib 30)))) (define f ?p1))");
  }
  
  public void testFuture()
  {
    convert("(future e1)", "(future e1)");
    convert("(future (+ e1 e2))", "(future (+ e1 e2))");
    convert("(future (+ e1 (* e2 e3)))", "(future (let ((?p0 (* e2 e3))) (+ e1 ?p0)))");
  }
  
  public void testSmallLets()
  {
    convert("(let ((f (lambda () f))) 'hello)", "(let ((f (lambda () f))) hello)");
    convert("(let* ((f (lambda () f))) 'hello)", "(let ((f (lambda () f))) hello)");
    convert("(letrec ((f (lambda () f))) 'hello)", "(letrec ((f (lambda () f))) hello)");
  }
  
}
