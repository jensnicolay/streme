package streme.lang.test;

import junit.framework.TestCase;
import streme.lang.data.DataUnifier;
import streme.lang.data.Parser2;
import streme.lang.eval.MacroExpander;

public class MacroExpanderTests extends TestCase
{
  private Parser2 parser = new Parser2();
  private DataUnifier unifier = new DataUnifier();
  
  public void testCore(String in, String expected)
  {
    MacroExpander expander = new MacroExpander();
    assertTrue(unifier.matches(parser.parse(expected), expander.rewrite(parser.parse(in))));
  }

  public void testLet(String in, String expected)
  {
    MacroExpander expander = new MacroExpander();
    expander.setLetToLambda(false);
    expander.setLetrecToLambda(false);
    assertTrue(unifier.matches(parser.parse(expected), expander.rewrite(parser.parse(in))));
  }

  public void testDefine()
  {
    testCore("(define x 28)", "(define x 28)");
    testCore("(define cond 28)", "(define cond 28)");
  }

  public void testLet()
  {
    testCore("(let ((a 1) (b 2)) (+ a b))", "((lambda (a b) (+ a b)) 1 2)");
  }

  public void testLetStar()
  {
    testCore("(let* ((a 1) (b 2)) (+ a b))", "((lambda (a) ((lambda (b) (+ a b)) 2)) 1)");
  }

  public void testLetrec()
  {
    testCore("(letrec ((a 1) (b 2)) (+ a b))", "((lambda (a b) (set! a 1) (set! b 2) (+ a b)) ?undefined ?undefined)");
  }

  public void testLambda()
  {
    testCore("(lambda (id x) x)", "(lambda (id x) x)");
    testCore("(lambda (id x) (define (inner y) (* y 2)) (inner x))",
        "(lambda (id x) ((lambda (inner) (set! inner (lambda (y) (* y 2))) (inner x)) ?undefined))");
    testLet("(define zulu-select (lambda (test lst) (define select-a (lambda (ac lst) (if (null? lst) (reverse! ac) (select-a 'a 'b)))) 'zulubody))", "(define zulu-select (lambda (test lst) (letrec ((select-a (lambda (ac lst) (if (null? lst) (reverse! ac) (select-a (quote a) (quote b)))))) (quote zulubody))))");
  }

  public void testAnd()
  {
    testCore("(and 'a 'b)", "(if (quote a) (quote b) #f)");
  }

  public void testOr()
  {
    testCore("(or 'a 'b)", "((lambda (?t) (if ?t ?t (quote b))) (quote a))");
  }

  public void testCond()
  {
    testCore("(cond ((number? x) 'number) ((pair? x) 'pair) (else 'help))",
        "(if (number? x) (quote number) (if (pair? x) (quote pair) (quote help)))");
  }

  public void testCase()
  {
    testCore(
        "(case x ((a b) 'number) ((c) 'pair) (else 'help)))",
        "((lambda (?t) (if (memv ?t (quote (a b))) (quote number) (if (memv ?t (quote (c))) (quote pair) (quote help)))) x)");
    testCore("(case x ((define) 'd))", "((lambda (?t) (if (memv ?t (quote (define))) (quote d) ?unspecified)) x)");
  }

  public void testBegin()
  {
    testCore("(begin (define x 28) (* x 2))", "(begin (define x 28) (* x 2))");
    testCore("(begin (let ((a 1)) a))", "(begin ((lambda (a) a) 1))");
  }

  public void testLetPar()
  {
    testCore("(let|| ((a exp1) (b exp2)) bodyexp)",
        "((lambda (a b) (set! a (touch a)) (set! b (touch b)) bodyexp) (future exp1) (future exp2))");
  }
  
  public void testLet2()
  {
    testLet("(let ((a 1) (b 2)) (and a b))", "(let ((a 1) (b 2)) (if a b #f))");
    testLet("(let ((a (let ((b (or x y))) (and a b)))) body)", "(let ((a (let ((b (let ((?t x)) (if ?t ?t y)))) (if a b #f)))) body)");
  }  
  
  public void testLetrec2()
  {
    testLet("(letrec ((f (f f))) (f f))", "(letrec ((f (f f))) (f f))");
  }
}
