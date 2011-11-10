package streme.lang.test;

import org.junit.Test;

public class R5RSTests extends StremeTestCase
{

  @Test
  public void test_1_3_4()
  {
    test("40", "(* 5 8)");
  }

  @Test
  public void test_4_1_1()
  {
    test("28", "(define x 28)", "x");
  }
  
  public void test_4_2_1()
  {
    test("greater", "(cond ((> 3 2) 'greater) ((< 3 2) 'less))");
    test("equal", "(cond ((> 3 3) 'greater) ((< 3 3) 'less) (else 'equal))");
  }
  
  public void test_6_1()
  {
    test("#t", "(equal? 'a 'a)");
    test("#t", "(equal? '(a) '(a))");
    test("#t", "(equal? '(a (b) c) '(a (b) c))");
    test("#t", "(equal? \"abc\" \"abc\")");
    test("#t", "(equal? 2 2)");
    // test("#t", "");
    test("#f", "(equal? (lambda (x) x) (lambda (y) y))");
  }
  
  public void test_6_4()
  {
    test("#(0 1 4 9 16)", "(let ((v (make-vector 5))) (for-each (lambda (i) (vector-set! v i (* i i))) '(0 1 2 3 4)) v)");
    test("-3", "(call-with-current-continuation (lambda (exit) (for-each (lambda (x) (if (negative? x) (exit x))) '(54 0 37 -3 245 19)) #t))");
    test("(4 . #f)", "(define list-length (lambda (obj) (call-with-current-continuation (lambda (return) (letrec ((r (lambda (obj) (cond ((null? obj) 0) ((pair? obj) (+ (r (cdr obj)) 1)) (else (return #f)))))) (r obj))))))", "(cons (list-length '(1 2 3 4)) (list-length '(a b . c)))");
  }
  
  public void test_4_2_4()
  {
    test("25", "(let ((x '(1 3 5 7 9))) (do ((x x (cdr x)) (sum 0 (+ sum (car x)))) ((null? x) sum)))");
    test("((6 1 3) (-5 -2))", "(let loop ((numbers '(3 -2 1 6 -5))" +
    		" (nonneg '()) (neg '())) (cond ((null? numbers) (list nonneg neg))" +
    		" ((>= (car numbers) 0) (loop (cdr numbers) (cons (car numbers) nonneg) neg))" +
    		" ((< (car numbers) 0) (loop (cdr numbers) nonneg (cons (car numbers) neg)))))");
    // TODO do loop in this section fails
  }
  
  public void test_6_3_2()
  {
    test("(101 102)", "(memv 101 '(100 101 102))");
  }
  
  public void test_6_2_5()
  {
    test("1", "(modulo 13 4)");
    test("1", "(remainder 13 4)");
    test("3", "(modulo -13 4)");
    test("-1", "(remainder -13 4)");
    test("-3", "(modulo 13 -4)");
    test("1", "(remainder 13 -4)");
    test("-1", "(modulo -13 -4)");
    test("-1", "(remainder -13 -4)");
  }
  
  public static void main(String[] args)
  {
    R5RSTests t = new R5RSTests();
    t.setUp();
    t.test_1_3_4();
  }
}
