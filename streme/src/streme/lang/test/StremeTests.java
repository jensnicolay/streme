package streme.lang.test;

import org.junit.Test;

public class StremeTests extends StremeTestCase
{

  @Test
  public void testCons()
  {
    test("(a . b)", "(cons 'a 'b)");
    test("(1 . 2)", "(cons '1 '2)");
    test("(\"a\" . \"b\")", "(cons \"a\" \"b\")");
  }

  public void testLambda()
  {
    test("25", "((lambda (x) (* x x)) 5)");
  }

  @Test
  public void testDefine()
  {
    test("28", "(define x 28)", "(define y x)", "y");
    test("16", "(define square (lambda (x) (* x x)))", "(square 4)");
    test("25", "(define (square2 x) (* x x))", "(square2 5)");
    test("3.14", "(define (id x) x)", "(id 3.14)");
  }

  @Test
  public void testNestedDefine()
  {
    test(
        "120",
        "(define (fact n) (define (fact-iter i acc) (if (zero? i) acc (fact-iter (- i 1) (* acc i)))) (fact-iter n 1))",
        "(fact 5)");
    test("28", "(define x 28)", "(define (test) (define x 29) 'yo)", "(test)", "x");
    test("(1 2 3)", "(define (t) (define (t2 a . b) (cons a b)) (t2 1 2 3))", "(t)");
  }

  @Test
  public void testArgBinding()
  {
    test("(0 3)", "(define (iter i j) (if (zero? i) (list i j) (iter (- i 1) (+ j 1))))", "(iter 1 2)");
  }

  @Test
  public void testMath()
  {
    test("6", "(+ 1 2 3)");
    test("33", "(- -2 -35)");
  }

  @Test
  public void testPreds()
  {
    test("#t", "(zero? 0)");
    test("#f", "(zero? 1)");
    // test("#t", "(zero? 0.0000)");
    // test("#f", "(zero? -0.00000001)");
    test("#t", "(null? '())");
    test("#f", "(null? '(1))");
  }

  @Test
  public void testAppend()
  {
    test("(1 2 3)", "(append '() '(1 2 3))");
    test("(1 2 3 4 5 6)", "(append '(1 2 3) '(4 5 6))");
    test("()", "(append '() '())");
    test("(1)", "(append '() '(1))");
    test("(1)", "(append '(1) '())");
    test("(a b c)", "(append '(a b c) '())");
    test("#f", "(define l '(1 2 3))", "(eq? l (append l '()))");
  }

  @Test
  public void testList()
  {
    test("()", "(list)");
    test("(1)", "(list 1)");
    test("(1 2)", "(list 1 2)");
  }

  @Test
  public void testIf()
  {
    test("true", "(if #t 'true 'false)");
    test("false", "(if #f 'true 'false)");
    test("ho", "(if 'hey 'ho 'hu)");
    test("ho", "(if 'hey 'ho)");
    test("<unspecified>", "(if #f 'ho)"); // FIXME cleanup unspecified/undefined
  }

  public void testAnd()
  {
    test("#t", "(and)");
    test("yo", "(and 'yo)");
    test("ho", "(and 'hey 'ho)");
    test("#f", "(and #f (/ 1 0))");
  }

  public void testOr()
  {
    test("#f", "(or)");
    test("yo", "(or 'yo)");
    test("hey", "(or 'hey 'ho)");
    test("#t", "(or #t (/ 1 0))");
  }

  public void testNot()
  {
    test("#f", "(not #t)");
    test("#f", "(not 'hey)");
    test("#t", "(not #f)");
  }

  @Test
  public void testFac()
  {
    test("24", "(define (fac n) (if (zero? n) 1 (* n (fac (- n 1)))))", "(fac 4)");
  }

  public void testLet()
  {
    test("(1 2 3)", "(define counter (let ((c 0)) (lambda () (set! c (+ c 1)) c)))",
        "(define a (counter))", "(define b (counter))" ,"(define c (counter))", "(list a b c)");
    test("84", "(define a 37)", "(let ((a 2) (b (+ a 5))) (* a b))");
    test("1", "(let () 1)");
    test("1", "(let ((a 1)) (let ((a 2) (b a)) b))");
  }

  @Test
  public void testLetStar()
  {
    test("24", "(define n 25)", "(let* ((n 24)) n)");
    test("48", "(define x 25)", "(let* ((x 24) (y 2)) (* x y))");
    test("100", "(let* ((a 10) (b (* a a))) b)");
    test("6", "(let* ((a 1) (b 2) (c 3)) (+ a b c))");
    test("3", "(let* () 3)");
  }

  public void testLetrec()
  {
    test("3", "(letrec ((a 1) (b 2)) (+ a b))");
    test("120", "(letrec ((fact (lambda (n) (if (zero? n) 1 (* n (fact (- n 1))))))) (fact 5))");
  }

  public void testEval()
  {
    test("5", "(eval '(define x 5) (interaction-environment))", "x");
  }

  public void testEq()
  {
    test("#t", "(eq? 'a 'a)");
    test("#t", "(eq? 1 1)");
    test("#f", "(eq? \"a\" \"a\")");
    test("#f", "(eq? (cons 'a 'b) (cons 'a 'b))");
  }

  public void testEqv()
  {
    test("#t", "(eqv? #t #t)");
    test("#t", "(eqv? #f #f)");
    test("#t", "(eqv? 'sym 'sym)");
    test("#t", "(eqv? 1 1)");
    test("#t", "(eqv? 1.2 1.2)");
    test("#t", "(eqv? '() '())");
    test("#t", "(eqv? eqv? eqv?)");
    test("#t", "(let ((c (cons 'a 'b))) (eqv? c c))");
    test("#t", "(let ((c \"a\")) (eqv? c c))");
    test("#f", "(eqv? 1 'a)");
    test("#f", "(eqv? #t #f)");
    test("#f", "(eqv? 'a 'aa)");
    test("#f", "(eqv? 1 1.0)");
    test("#f", "(eqv? 1 1.0)");
    test("#f", "(eqv? '() #f)");
    test("#f", "(eqv? eq? eqv?)");
    test("#f", "(eqv? (cons 'a 'b) (cons 'a 'b))");
    test("#f", "(eqv? \"a\" \"a\")");
    
    // PLT rules
    test("#f", "(eqv? \"\" \"\")");
    test("#f", "(eqv? (lambda (x) x) (lambda (x) x))");
  }

  public void testEquality()
  {
    test("#t", "(= 1 1)");
    test("#f", "(= 1 2)");
    test("#t", "(= 1.0 1.0)");
    test("#f", "(= 1.0 2.0)");
    test("#t", "(= 1/3 1/3)");
    test("#f", "(= 1/3 2/3)");
    // test("#t", "(= 1 1.0)"); TODO PLT returns #t
  }

  public void testLessThan()
  {
    test("#t", "(< 3 5)");
    test("#f", "(< 5 3)");
    test("#f", "(< 5 5)");
  }

  public void testStringLessThanP()
  {
    test("#t", "(string<? \"abc\" \"bac\")");
    test("#f", "(string<? \"bac\" \"abc\")");
    test("#f", "(string<? \"abc\" \"abc\")");
  }

  public void testLessThanOrEquals()
  {
    test("#t", "(<= 3 5)");
    test("#f", "(<= 5 3)");
    test("#t", "(<= 5 5)");
  }

  public void testGreaterThan()
  {
    test("#f", "(> 3 5)");
    test("#t", "(> 5 3)");
    test("#f", "(> 5 5)");
  }

  public void testGreaterThanOrEquals()
  {
    test("#f", "(>= 3 5)");
    test("#t", "(>= 5 3)");
    test("#t", "(>= 5 5)");
  }

  public void testBegin()
  {
    test("5", "(begin (define x 5) x)", "x");
    test("123", "(define f (lambda () (begin (define v 123)) v))", "(f)");
    test("225", "(begin (define (f) (* u u)) (define u 15) (f))");
  }

  public void testEnvHandling()
  {
    test("28", "(define (test) x)", "(define x 28)", "(test)");
  }

  public void testCarCdr()
  {
    test("1", "(car '(1 2 3))");
    test("(2 3)", "(cdr '(1 2 3))");
    test("()", "(cdr '(1))");
  }
  
  public void testListRef()
  {
    test("1", "(list-ref '(1 2 3) 0)");
    test("2", "(list-ref '(1 2 3) 1)");
    test("3", "(list-ref '(1 2 3) 2)");
  }
  
  public void testMap()
  {
    test("(1 4 9)", "(map (lambda (x) (* x x)) '(1 2 3))");
  }
  
  public void testCond()
  {
    test(UNDEFINED, "(cond)");
    test("hehe", "(cond ('hehe))");
    test("hehe", "(cond (#t 'hehe))");
    test("hehe", "(cond (#t 'hihi 'hehe))");
    test("hoho", "(cond (#f 'hehe) (else 'hoho))");
    test("hoho", "(cond (#f 'hehe) (else 'hihi 'hoho))");
  }

  public void testCase()
  {
    test(UNDEFINED, "(case 'key)");
    test("hehe", "(case 'key ((key) 'hehe))");
    test("hehe", "(case 'key ((koi key) 'hehe))");
    test("hehe", "(case 'key ((koi key) 'hihi 'hehe))");
    test("hehe", "(case 'kai ((koi key) 'hoho) (else 'hehe))");
    test("hehe", "(case 'kai ((koi key) 'hoho) (else 'hihi 'hehe))");
  }

  public void testVarArg()
  {
    test("(1)", "(define t1 (lambda (f . r) (cons f r)))", "(t1 1)");
    test("(1 2 3)", "(t1 1 2 3)");
    test("(1 2)", "(define t2 (lambda (f g . r) (cons f (cons g r))))", "(t2 1 2)");
    test("(1 2 3 4)", "(t2 1 2 3 4)");
    test("(1 2 3 4 5 6)", "(define tn (lambda (f g h i j k . r) (cons f (cons g (cons h (cons i (cons j (cons k r))))))))", "(tn 1 2 3 4 5 6)");
    test("(1 2 3 4 5 6 7 8 9)", "(tn 1 2 3 4 5 6 7 8 9)");
    test("(1)", "(define (t3 f . r) (cons f r))", "(t3 1)");
    test("(1 2 3)", "(t3 1 2 3)");
    test("(1 2)", "(define (t4 f g . r) (cons f (cons g r)))", "(t4 1 2)");
    test("(1 2 3 4)", "(t4 1 2 3 4)");
    test("(1 2 3 4 5 6)", "(define (tm f g h i j k . r) (cons f (cons g (cons h (cons i (cons j (cons k r)))))))", "(tm 1 2 3 4 5 6)");
    test("(1 2 3 4 5 6 7 8 9)", "(tm 1 2 3 4 5 6 7 8 9)");
    test("(1 2 3)", "(define tt (lambda r r))", "(tt 1 2 3)");
    test("(1 2 3)", "(define (ttt . r) r)", "(ttt 1 2 3)");
    test("()", "(tt)");
    test("()", "(ttt)");
  }
  
  public void testLength()
  {
    test("0", "(length '())");
    test("1", "(length '(1))");
    test("2", "(length '(1 2))");
  }
  
  public void testDo()
  {
    test("#(0 1 2 3)", "(let ((v (make-vector 4))) (do ((m 0 (+ m 1))) ((> m 3)) (vector-set! v m m)) v)");
    test("(3 2 1 0)", "(do ((a '()) (m 0 (+ m 1))) ((> m 3) a) (set! a (cons m a)))");
  }
  
  public void testModulo()
  {
    test("0", "(modulo 1 -1)");
    test("0", "(modulo -1 1)");
  }
  
  public void testQuasiQuote()
  {
    test("3", "`3");
    test("a", "`a");
    test("()", "`()");
    test("6", "(+ `3 `3)");
    test("#t", "(list? `(1 2 3))");
    test("(1 (1 2 3) 3)", "`(1 ,'(1 2 3) 3)");
    test("(+ 1 2)", "`(+ 1 2)");
    test("(1 2 3)", "(let ((a 1)) `(,a 2 3))");
    test("(1 2 3 4)", "`(1 ,@'(2 3) 4)");
    test("(proxy (map + (list 1 2 3)))", "`(proxy (map + (list ,@'(1 2 3))))");
    
    test("(1 2 . 3)", "`(1 2 . 3)");
    /* Not well-behaved for unquoting in last cdr of improper list, gives (1 2 unquote a)
     * while Racket evaluates to (1 2 . 3), BUT Racket evaluates '(1 2 . `(3 4))
     * to (1 2 quasiquote (3 4)), like Streme does: same type of problem, so won't fix for now.
     */ 
    //test("(1 2 . 3)", "(let ((a 3)) `(1 2 . ,a))");
    
    // TODO nested quasi-quoting doesn't work !!!
    //test("`(cons amb-thunks ,@(map (lambda (x) `(lambda () ,x)) '(1 2 3)))", "(cons amb-thunks (lambda () 1) (lambda () 2) (lambda () 3))");
    
  }
    
  public void testMisc()
  {
    test("15", "(letrec ((sum (lambda (ls) (if (null? ls) 0 (+ (car ls) (sum (cdr ls))))))) (sum '(1 2 3 4 5)))");
    test(
        "(#t #f)",
        "(letrec ((even? (lambda (x) (or (= x 0) (odd? (- x 1))))) (odd? (lambda (x) (and (not (= x 0)) (even? (- x 1)))))) (list (even? 20) (odd? 20)))");
    test("(1 2 3 4)", "(define my-append (lambda (ls1 ls2) (if (null? ls1) ls2 (cons (car ls1) (my-append (cdr ls1) ls2)))))", "(my-append '(1 2) '(3 4))");
    test("120", "(letrec ((f (lambda (x k) (if (zero? x) (k 1) (f (- x 1) (lambda (v) (k (* x v)))))))) (f 5 (lambda (x) x)))");
    test("2", "(define (cpstak x y z) (define (tak x y z k) (if (not (< y x)) (k z) (tak (- x 1) y z (lambda (v1) (tak (- y 1) z x (lambda (v2) (tak (- z 1) x y (lambda (v3) (tak v1 v2 v3 k))))))))) (tak x y z (lambda (a) a)))",
        "(cpstak 10 4 1)");
    test("100", "(define (create-n n) (do ((n n (- n 1)) (a '() (cons '() a))) ((= n 0) a)))", "(define *ll* (create-n 200))", "(define (iterative-div2 l) (do ((l l (cddr l)) (a '() (cons (car l) a))) ((null? l) a)))", "(length (iterative-div2 *ll*))");
  }
 
}
