package streme.lang.test;

import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.ExecutorService;

import jsr166y.ForkJoinPool;
import junit.framework.TestCase;
import streme.lang.Logging;
import streme.lang.ast.AstDataCompiler;
import streme.lang.ast.Node;
import streme.lang.ast.analysis.AnfConverter;
import streme.lang.ast.analysis.RenamingStrategy;
import streme.lang.ast.analysis.ipda.BruteForceParallelizer2;
import streme.lang.ast.analysis.ipda.Ipda;
import streme.lang.ast.impl.StremeDataCompiler;
import streme.lang.data.DataUnifier;
import streme.lang.data.Pair;
import streme.lang.data.Parser2;
import streme.lang.eval.MacroExpander;
import streme.lang.eval.tanfe.TanfStreme;

public class BruteForceParallelizerTests extends TestCase
{
  static
  {
    Logging.setup();
  }
  Parser2 parser = new Parser2();
  DataUnifier unifier = new DataUnifier();

  private Set<Object> setOf(Object... el)
  {
    Set<Object> s = new HashSet<Object>();
    for (Object e : el)
    {
      s.add(e);
    }
    return s;
  }

  private Node convertToAnf(String source)
  {
    Object data = parser.parse(source);
    AstDataCompiler dataCompiler = new StremeDataCompiler();
    MacroExpander expander = new MacroExpander();
    expander.setLetToLambda(false);
    expander.setLetrecToLambda(false);
    Node ast = dataCompiler.compile(expander.rewrite(data));
    AnfConverter anfConverter = new AnfConverter(RenamingStrategy.NUMBER_RENAMING_STRATEGY);
    Node anf = anfConverter.rewrite(ast);
    return anf;
  }

  private Ipda doAnalysis(Node anf)
  {
    Ipda ipda = new Ipda(1);
    ipda.analyze(anf);
    return ipda;
  }

  private Node parallelize(String source)
  {
    Node anf = convertToAnf(source);
    Ipda ipda = doAnalysis(anf);
    BruteForceParallelizer2 bfp = new BruteForceParallelizer2(ipda);
    Node rewritten = bfp.rewrite(anf);
//    boolean matches = unifier.matches(parser.parse(expected), rewritten.toData());
//    assertTrue(expected, matches);
    return rewritten;
  }

  private Object eval(Node p)
  {
    ExecutorService executor = new ForkJoinPool();
    TanfStreme streme = new TanfStreme(executor);
    Object result = streme.evaluateData(p.toData(), streme.globalEnv());
    executor.shutdown();
    return result;
  }

//  public void test1()
//  {
//    parallelize(
//        "(let* ((z 0) (writez (lambda () (set! z 123))) (readz (lambda () z))) (h (writez) (readz)))",
//        "(let ((z 0)) (let ((writez (lambda () (set! z 123)))) (let ((readz (lambda () z))) (let ((?p0 (writez))) (let ((?p1 (readz))) (h ?p0 ?p1))))))");
//    parallelize(
//        "(let* ((z 0) (writez (lambda () (set! z 123))) (readz (lambda () z))) (h (readz) (writez)))",
//        "(let ((z 0)) (let ((writez (lambda () (set! z 123)))) (let ((readz (lambda () z))) (let ((?p0 (readz))) (let ((?p1 (writez))) (h ?p0 ?p1))))))");
//    parallelize(
//        "(let* ((z 0) (writez (lambda () (set! z 123))) (readz (lambda () z))) (h (readz) (readz)))",
//        "(let ((z 0)) (let ((writez (lambda () (set! z 123)))) (let ((readz (lambda () z))) (let|| ((?p0 (readz)) (?p1 (readz))) (h ?p0 ?p1)))))");
//    parallelize(
//        "(let* ((z 0) (writez (lambda () (set! z 123))) (readz (lambda () z))) (h (writez) (writez)))",
//        "(let ((z 0)) (let ((writez (lambda () (set! z 123)))) (let ((readz (lambda () z))) (let ((?p0 (writez))) (let ((?p1 (writez))) (h ?p0 ?p1))))))");
//  }
  
  private void testAnswer(Node p, Object expected)
  {
    for (int i = 0; i < 5; i++)
    {
      Object v = eval(p);
      assertEquals(expected, v);
    }
  }
  
  private void testContainsLetpar(Node p)
  {
    assertTrue(p.toString().contains("(let||"));
  }
  
  private void testNotContainsLetpar(Node p)
  {
    assertFalse(p.toString().contains("(let||"));
  }
  
  public void testFib()
  {
    String source = "(letrec ((fib (lambda (n)  (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))))) (fib 20))";
    Node p = parallelize(source);
    testContainsLetpar(p);
    testAnswer(p, 6765);
  }

  public void testFac()
  {
    String source = "(letrec ((fact (lambda (n) (if (zero? n) 1 (* n (fact (- n 1))))))) (fact 10))";
    Node p = parallelize(source);
    testNotContainsLetpar(p);
    testAnswer(p, 3628800);
  }
  
  public void testSimple1()
  {
    String source = "(let* ((z 0) (writez (lambda () (set! z 123))) (readz (lambda () z))) (cons (writez) (readz)))";
    Node p = parallelize(source);
    testContainsLetpar(p);
    testAnswer(p, Pair.cons(Void.TYPE, 123));
  }
  
  public void testSimple2()
  {
    String source = "(let* ((z 0) (readz (lambda () z))) (cons (readz) (readz)))";
    Node p = parallelize(source);
    testContainsLetpar(p);
    testAnswer(p, Pair.cons(0, 0));
  }
  
  public void test3()
  {
    String source = "(let ((h (lambda (n) (* n n)))) (let ((g (lambda (m) (* m m m)))) (let ((n1 2)) (let ((n2 (h n1))) (let ((n3 (g n1))) (let ((n4 (* n3 100))) (* n2 n4)))))))";
    Node p = parallelize(source);
    testContainsLetpar(p);
    testAnswer(p, 3200);
  }
   
  public void testTak()
  {
    String source = "(letrec ((tak (lambda (x y z) (if (not (< y x)) z (tak (tak (- x 1) y z) (tak (- y 1) z x) (tak (- z 1) x y)))))) (tak 10 4 1))";
    Node p = parallelize(source);
    testContainsLetpar(p);
    testAnswer(p, 2);
  }
   
  public void testDangling1()
  {
    String source = "(let* ((z 0) (x (+ z 1)) (y (+ z 4)) (u (+ x y)) (u1 (+ u 9)) (u2 (+ u 10)) (uu (* u1 u2)) (r (* x y))) r)";    
    Node p = parallelize(source);
    testContainsLetpar(p);
    testAnswer(p, 4);
  }
  
  public void testDangling2()
  {
    String source = "(let* ((z 0) (s (+ z 2)) (x (+ z 5)) (y (+ z 7)) (u (+ s x y)) (u1 (+ u 11)) (u2 (+ u 13)) (r (* x y))) r)";
    Node p = parallelize(source);
    testContainsLetpar(p);
    testAnswer(p, 35);
  }
  
  public void testDangling3()
  {
    String source = "(let* ((z 0) (s (+ z 3)) (x (+ z 6)) (y (+ z 8)) (u (+ s x)) (u1 (+ u 12)) (u2 (+ u y)) (r (* x y))) r)";    
    Node p = parallelize(source);
    testContainsLetpar(p);
    testAnswer(p, 48);
  }
  
  public void testBegin()
  {
    String source = "(let ((z 0)) (let ((writez (lambda () (set! z 123)))) (let ((readz (lambda () z))) (begin (writez) (readz)))))";
    Node p = parallelize(source);
    testContainsLetpar(p);
    testAnswer(p, 123);
  }

  public void test1()
  {
    String source="(let ((l '())) (letrec ((f (lambda (i) (if (zero? i) i (set! l 123))))) (f 10) l))";
    Node p = parallelize(source);
    testNotContainsLetpar(p);
    testAnswer(p, 123);
  }

  public void test2()
  {
    String source="(let ((l '())) (letrec ((f (lambda (i) (if (zero? i) i (begin (set! l (cons i l)) (f (- i 1))))))) (f 10) (f 10) l)))";
    Node p = parallelize(source);
    testContainsLetpar(p);
    testAnswer(p, parser.parse("(1 2 3 4 5 6 7 8 9 10 1 2 3 4 5 6 7 8 9 10)"));
  }

}
