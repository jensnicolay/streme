package streme.lang.test;

import junit.framework.TestCase;
import streme.lang.Logging;
import streme.lang.analysis.DependenceAnalyzer;
import streme.lang.analysis.IpdAnalysis;
import streme.lang.analysis.IpdAnalyzer;
import streme.lang.analysis.ParentAnalysis;
import streme.lang.analysis.ParentAnalyzer;
import streme.lang.analysis.VarPointerAnalysis;
import streme.lang.analysis.VarPointerAnalyzer;
import streme.lang.ast.AstDataCompiler;
import streme.lang.ast.Node;
import streme.lang.ast.impl.StremeDataCompiler;
import streme.lang.data.Parser2;

public class DependenceAnalyzerTests extends TestCase
{
  static
  {
    Logging.setup();
  }
  Parser2 parser = new Parser2();

  private DependenceAnalyzer doAnalysis(Node ast, int k)
  {
    ParentAnalysis parentAnalysis = new ParentAnalyzer().analyze(ast);
    VarPointerAnalysis varPointerAnalysis = new VarPointerAnalyzer(parentAnalysis).analyze(ast);
    IpdAnalysis ipdAnalysis = new IpdAnalyzer(k, true, varPointerAnalysis).analyze(ast);
    DependenceAnalyzer dependenceAnalyzer = new DependenceAnalyzer(varPointerAnalysis, ipdAnalysis);
    return dependenceAnalyzer;
  }

  public Node createAst(String source)
  {
    Object data = parser.parse(source);
    AstDataCompiler dataCompiler = new StremeDataCompiler();
    Node ast = dataCompiler.compile(data);
    return ast;
  }

  public void testFib()
  {
    String source = "(letrec ((fib (lambda (n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))))) (fib 20))";
    Node ast = createAst(source);
    DependenceAnalyzer dependenceAnalyzer = doAnalysis(ast, 12);
  }

  public void testFibWithLet()
  {
    String source = "(letrec ((fib (lambda (n) (if (< n 2) n (let ((a (fib (- n 2))) (b (fib (- n 1)))) (+ a b)))))) (fib 4))";
    Node ast = createAst(source);
    DependenceAnalyzer dependenceAnalyzer = doAnalysis(ast, 12);
  }
  
  // public void testFac()
  // {
  // String source = "(letrec ((fact (lambda (n) (if (zero? n) 1 (* n (fact (- n 1))))))) (fact 10))";
  // Node p = parallelize(source);
  // testNotContainsLetpar(p);
  // testAnswer(p, 3628800);
  // }
  //
  // public void testSimple1()
  // {
  // String source = "(let* ((z 0) (writez (lambda () (set! z 123))) (readz (lambda () z))) (cons (writez) (readz)))";
  // Node p = parallelize(source);
  // testContainsLetpar(p);
  // testAnswer(p, Pair.cons(Void.TYPE, 123));
  // }
  //
  // public void testSimple2()
  // {
  // String source = "(let* ((z 0) (readz (lambda () z))) (cons (readz) (readz)))";
  // Node p = parallelize(source);
  // testContainsLetpar(p);
  // testAnswer(p, Pair.cons(0, 0));
  // }
  //
  // public void test3()
  // {
  // String source =
  // "(let ((h (lambda (n) (* n n)))) (let ((g (lambda (m) (* m m m)))) (let ((n1 2)) (let ((n2 (h n1))) (let ((n3 (g n1))) (let ((n4 (* n3 100))) (* n2 n4)))))))";
  // Node p = parallelize(source);
  // testContainsLetpar(p);
  // testAnswer(p, 3200);
  // }
  //
  // public void testTak()
  // {
  // String source =
  // "(letrec ((tak (lambda (x y z) (if (not (< y x)) z (tak (tak (- x 1) y z) (tak (- y 1) z x) (tak (- z 1) x y)))))) (tak 10 4 1))";
  // Node p = parallelize(source);
  // testContainsLetpar(p);
  // testAnswer(p, 2);
  // }
  //
  // public void testDangling1()
  // {
  // String source =
  // "(let* ((z 0) (x (+ z 1)) (y (+ z 4)) (u (+ x y)) (u1 (+ u 9)) (u2 (+ u 10)) (uu (* u1 u2)) (r (* x y))) r)";
  // Node p = parallelize(source);
  // testContainsLetpar(p);
  // testAnswer(p, 4);
  // }
  //
  // public void testDangling2()
  // {
  // String source =
  // "(let* ((z 0) (s (+ z 2)) (x (+ z 5)) (y (+ z 7)) (u (+ s x y)) (u1 (+ u 11)) (u2 (+ u 13)) (r (* x y))) r)";
  // Node p = parallelize(source);
  // testContainsLetpar(p);
  // testAnswer(p, 35);
  // }
  //
  // public void testDangling3()
  // {
  // String source =
  // "(let* ((z 0) (s (+ z 3)) (x (+ z 6)) (y (+ z 8)) (u (+ s x)) (u1 (+ u 12)) (u2 (+ u y)) (r (* x y))) r)";
  // Node p = parallelize(source);
  // testContainsLetpar(p);
  // testAnswer(p, 48);
  // }
  //
  // public void testBegin()
  // {
  // String source =
  // "(let ((z 0)) (let ((writez (lambda () (set! z 123)))) (let ((readz (lambda () z))) (begin (writez) (readz)))))";
  // Node p = parallelize(source);
  // testContainsLetpar(p);
  // testAnswer(p, 123);
  // }
  //
  // public void test1()
  // {
  // String source="(let ((l '())) (letrec ((f (lambda (i) (if (zero? i) i (set! l 123))))) (f 10) l))";
  // Node p = parallelize(source);
  // testNotContainsLetpar(p);
  // testAnswer(p, 123);
  // }
  //
  // public void test2()
  // {
  // String
  // source="(let ((l '())) (letrec ((f (lambda (i) (if (zero? i) i (begin (set! l (cons i l)) (f (- i 1))))))) (f 10) (f 10) l)))";
  // Node p = parallelize(source);
  // testContainsLetpar(p);
  // testAnswer(p, parser.parse("(1 2 3 4 5 6 7 8 9 10 1 2 3 4 5 6 7 8 9 10)"));
  // }
}
