package streme.lang.test;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import junit.framework.TestCase;
import streme.lang.Logging;
import streme.lang.ast.Application;
import streme.lang.ast.AstDataCompiler;
import streme.lang.ast.Node;
import streme.lang.ast.analysis.AnfConverter;
import streme.lang.ast.analysis.LetBodyApplicationRemover;
import streme.lang.ast.analysis.RenamingStrategy;
import streme.lang.ast.analysis.ipda.ContextMark;
import streme.lang.ast.analysis.ipda.Ipda;
import streme.lang.ast.analysis.ipda.Ipda.Dependency;
import streme.lang.ast.analysis.ipda.Mark;
import streme.lang.ast.analysis.kcfa.Binding;
import streme.lang.ast.impl.NodeFinders;
import streme.lang.ast.impl.StremeDataCompiler;
import streme.lang.data.Parser2;
import streme.lang.data.Sym;
import streme.lang.eval.MacroExpander;

public class IpdaTests extends TestCase
{
  static
  {
    Logging.setup();
  }
  private Parser2 parser = new Parser2();

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
    MacroExpander expander = new MacroExpander();
    expander.setLetToLambda(false);
    expander.setLetrecToLambda(false);
    AstDataCompiler dataCompiler = new StremeDataCompiler();
    Node ast = dataCompiler.compile(expander.rewrite(data));
    AnfConverter anfConverter = new AnfConverter(RenamingStrategy.NUMBER_RENAMING_STRATEGY);
    Node anf = anfConverter.rewrite(ast);
    LetBodyApplicationRemover lbar = new LetBodyApplicationRemover(RenamingStrategy.NUMBER_RENAMING_STRATEGY);
    Node lbarAnf = lbar.rewrite(anf);
    return lbarAnf;
  }

  private Ipda doAnalysis(Node anf)
  {
    return doAnalysis(anf, 1);
  }

  private Ipda doAnalysis(Node anf, int k)
  {
    Ipda ipda = new Ipda(k);
    ipda.analyze(anf);
    return ipda;
  }

  private Mark findMark(Ipda ipda, String p1)
  {
    return (Mark) findMark(ipda, p1, "?c");
  }

  private Mark findMark(Ipda ipda, String p1, String context)
  {
    return (Mark) ipda.findVertex(parser.parse("(mark " + p1 + " " + context + ")"));
  }

  private Binding findBinding(Ipda ipda, String p2)
  {
    return (Binding) ipda.findVertex(parser.parse("(binding " + p2 + " ?c)"));
  }

  private Set<Dependency> getDependencies(Ipda ipda, String p1, String p2)
  {
    Mark n1 = findMark(ipda, p1);
    Mark n2 = findMark(ipda, p2);
    return ipda.getDependencies(n1, n2);
  }

  private boolean reads(Ipda ipda, String p1, String p2)
  {
    Mark n1 = findMark(ipda, p1);
    Binding n2 = findBinding(ipda, p2);
    return ipda.reads(n1, n2);
  }

  private boolean writes(Ipda ipda, String p1, String p2)
  {
    Mark n1 = findMark(ipda, p1);
    Binding n2 = findBinding(ipda, p2);
    return ipda.writes(n1, n2);
  }

  public void test1()
  {
    String source = "(let* ((z 0) (writez (lambda () (set! z 123))) (readz (lambda () z)) (a (readz)) (b (writez))) (h a b))";
    Node anf = convertToAnf(source);
    Ipda ipda = doAnalysis(anf);
    assertEquals(setOf(Dependency.WR), getDependencies(ipda, "(lambda () (set! . ?))", "(lambda () z)"));
    assertEquals(setOf(Dependency.RW), getDependencies(ipda, "(lambda () z)", "(lambda () (set! . ?))"));
    assertFalse(reads(ipda, "(lambda () (set! . ?))", "z"));
    assertTrue(reads(ipda, "(lambda () z)", "z"));
    assertFalse(writes(ipda, "(lambda () z)", "z"));
  }

  public void test1b()
  {
    String source = "(let* ((z 0) (writez (lambda (r) (set! z 123))) (readz (lambda (s) z)) (a (readz 1)) (b (writez 2))) (h a b))";
    Node anf = convertToAnf(source);
    Ipda ipda = doAnalysis(anf);
    assertEquals(setOf(Dependency.WR), getDependencies(ipda, "(lambda (r) . ?)", "(lambda (s) . ?)"));
    assertEquals(setOf(Dependency.RW), getDependencies(ipda, "(lambda (s) . ?)", "(lambda (r) . ?)"));
    assertTrue(writes(ipda, "(lambda (r) . ?)", "z"));
    assertFalse(reads(ipda, "(lambda (r) . ?)", "z"));
    assertTrue(reads(ipda, "(lambda (s) . ?)", "z"));
    assertFalse(writes(ipda, "(lambda (s) . ?)", "z"));
  }

  public void test1c()
  {
    String source = "(let* ((z 0) (zz 1) (writez (lambda () (set! zz 123))) (readz (lambda () z)) (a (readz)) (b (writez))) (h a b))";
    Node anf = convertToAnf(source);
    Ipda ipda = doAnalysis(anf);
    assertEquals(setOf(), getDependencies(ipda, "(lambda () (set! zz 123))", "(lambda () z)"));
    assertEquals(setOf(), getDependencies(ipda, "(lambda () z)", "(lambda () (set! zz 123))"));
    assertTrue(writes(ipda, "(lambda () (set! zz 123))", "zz"));
    assertFalse(writes(ipda, "(lambda () (set! zz 123))", "z"));
    assertTrue(reads(ipda, "(lambda () z)", "z"));
    assertFalse(reads(ipda, "(lambda () z)", "zz"));
  }

  public void test1d()
  {
    String source = "(let ((z 0)) (let ((writez (lambda () (set! z 123))) (readz (lambda () z))) (let ((a (readz)) (b (writez))) (h a b))))";
    Node anf = convertToAnf(source);
    Ipda ipda = doAnalysis(anf);
    assertEquals(setOf(Dependency.WR), getDependencies(ipda, "(lambda () (set! z 123))", "(lambda () z)"));
    assertEquals(setOf(Dependency.RW), getDependencies(ipda, "(lambda () z)", "(lambda () (set! z 123))"));
  }

  public void test2()
  {
    String source = "(let* ((z 0) (writez (lambda () (set! z 123))) (readz (lambda () z)) (b (writez)) (a (readz))) (h a b))";
    Node anf = convertToAnf(source);
    Ipda ipda = doAnalysis(anf);
    assertEquals(setOf(Dependency.WR), getDependencies(ipda, "(lambda () (set! z 123))", "(lambda () z)"));
    assertEquals(setOf(Dependency.RW), getDependencies(ipda, "(lambda () z)", "(lambda () (set! z 123))"));
  }

  public void test3()
  {
    String source = "(let* ((z 0) (writez (lambda () (set! z 123))) (readz (lambda () z))) (if #t (writez) (readz)))";
    Node anf = convertToAnf(source);
    Ipda ipda = doAnalysis(anf);
    assertTrue(writes(ipda, "(lambda () (set! z 123))", "z"));
    assertFalse(reads(ipda, "(lambda () (set! z 123))", "z"));
    //assertEquals(setOf((Object) null), ipda.getAnswer());
  }

  public void test3b()
  {
    String source = "(let* ((z 0) (writez (lambda () (set! z 123))) (readz (lambda () z))) (if #f (writez) (readz)))";
    Node anf = convertToAnf(source);
    Ipda ipda = doAnalysis(anf);;
    assertFalse(writes(ipda, "(lambda () z)", "z"));
    assertTrue(reads(ipda, "(lambda () z)", "z"));
    //assertEquals(setOf(0), ipda.getAnswer());
  }

  public void test4()
  {
    String source = "(let* ((z 0) (writez (lambda () (set! z 123))) (t (writez))) z)";
    Node anf = convertToAnf(source);
    Ipda ipda = doAnalysis(anf);
    //assertEquals(setOf(123), ipda.getAnswer());
  }

  public void test5()
  {
    String source = "(let* ((z 0) (writez (lambda () (set! z 123))) (readz (lambda () z))) (h (readz) (writez)))";
    Node anf = convertToAnf(source);
    Ipda ipda = doAnalysis(anf);
    assertEquals(setOf(Dependency.WR), getDependencies(ipda, "(lambda () (set! z 123))", "(lambda () z)"));
    assertEquals(setOf(Dependency.RW), getDependencies(ipda, "(lambda () z)", "(lambda () (set! z 123))"));
  }
  

  public void testCpsy()
  {
    String source = "(let ((id (lambda (x q) (q x)))) (id 3 (lambda (v1) (id 4 (lambda (v2) (halt v2))))))";
    Node anf = convertToAnf(source);
    Object lambda1 = NodeFinders.findUnifyingNode(anf, parser.parse("(lambda (x q) ?)"));
    Object lambda2 = NodeFinders.findUnifyingNode(anf, parser.parse("(lambda (v1) ?)"));
    Object lambda3 = NodeFinders.findUnifyingNode(anf, parser.parse("(lambda (v2) ?)"));
    Ipda ipda = doAnalysis(anf);
    assertEquals(setOf(lambda1), ipda.flowsTo(new Sym("id")));
    assertEquals(setOf(lambda2, lambda3), ipda.flowsTo(new Sym("q")));
    assertEquals(setOf(3, 4), ipda.flowsTo(new Sym("x")));
    assertEquals(setOf(3), ipda.flowsTo(new Sym("v1")));
    assertEquals(setOf(4), ipda.flowsTo(new Sym("v2")));
    ipda = doAnalysis(anf, 0); 
    assertEquals(setOf(lambda1), ipda.flowsTo(new Sym("id")));
    assertEquals(setOf(lambda2, lambda3), ipda.flowsTo(new Sym("q")));
    assertEquals(setOf(3, 4), ipda.flowsTo(new Sym("x")));
    assertEquals(setOf(3, 4), ipda.flowsTo(new Sym("v1")));
    assertEquals(setOf(3, 4), ipda.flowsTo(new Sym("v2")));
  }
  

  public void testPrimopPlus()
  {
    String source = "(let ((z 0)) (+ z 1))";
    Node anf = convertToAnf(source);
    Ipda ipda = doAnalysis(anf);
    //assertEquals(setOf(1), ipda.getAnswer());
    source = "(let ((z 0)) (+ z 1 2))";
    anf = convertToAnf(source);
    ipda = doAnalysis(anf);
    //assertEquals(setOf(3), ipda.getAnswer());
  }
  
  public void testPrimopPlus2()
  {
    String source = "(let* ((z 0) (writez (lambda () (set! z (+ z 1))))) (writez))";
    Node anf = convertToAnf(source);
    Ipda ipda = doAnalysis(anf);
    //assertEquals(setOf((Object) null), ipda.getAnswer());
    assertTrue(writes(ipda, "(lambda () . ?)", "z"));
    assertTrue(reads(ipda, "(lambda () . ?)", "z"));
  }
  
  public void testPrimopZeroP()
  {
    String source = "(let ((z 0)) (zero? z))";
    Node anf = convertToAnf(source);
    Ipda ipda = doAnalysis(anf);
    //assertEquals(setOf(Boolean.TRUE), ipda.getAnswer());
    source = "(let ((z 1)) (zero? z))";
    anf = convertToAnf(source);
    ipda = doAnalysis(anf);
    //assertEquals(setOf(Boolean.FALSE), ipda.getAnswer());
    source = "(let ((z 1)) (zero? x))";
    anf = convertToAnf(source);
    ipda = doAnalysis(anf);
    //assertEquals(setOf(Boolean.TRUE, Boolean.FALSE), ipda.getAnswer());
  }
  
  public void testPrimopMinus()
  {
    String source = "(let ((z 10)) (- z 1))";
    Node anf = convertToAnf(source);
    Ipda ipda = doAnalysis(anf);
    //assertEquals(setOf(9), ipda.getAnswer());
    source = "(let ((z 10)) (- z 1 2))";
    anf = convertToAnf(source);
    ipda = doAnalysis(anf);
    //assertEquals(setOf(7), ipda.getAnswer());
  }
  
  public void testLetrec0()
  {
    String source = "(letrec ((f (lambda (n) (if (zero? n) 0 (f (- n 1)))))) (f 0))";
    Node anf = convertToAnf(source);
    Ipda ipda = doAnalysis(anf);
    //assertEquals(setOf(0), ipda.getAnswer());
  }

  public void testLetrec1()
  {
    String source = "(letrec ((f (lambda (n) (if (zero? n) 0 (f (- n 1)))))) (f 1))";
    Node anf = convertToAnf(source);
    Ipda ipda = doAnalysis(anf);
    //assertEquals(setOf(0), ipda.getAnswer());
  }

  public void testLetrec2()
  {
    String source = "(letrec ((f (lambda (n) (if (zero? n) 0 (f (- n 1)))))) (f 2))";
    Node anf = convertToAnf(source);
    Ipda ipda = doAnalysis(anf, 1);
    //assertEquals(setOf(0), ipda.getAnswer());
    ipda = doAnalysis(anf, 3);
    //assertEquals(setOf(0), ipda.getAnswer());
    ipda = doAnalysis(anf, 4);
    //assertEquals(setOf(0), ipda.getAnswer());
  }
  
  public void testFact()
  {
    String source = "(letrec ((fact (lambda (n) (if (zero? n) 1 (* n (fact (- n 1))))))) (fact 3))";
    Node anf = convertToAnf(source);
    Ipda ipda = doAnalysis(anf, 1);
    //assertEquals(setOf(3), ipda.getAnswer());
    List<ContextMark> marks = ipda.findVertices(parser.parse("(mark (lambda . ?) ?context)"), ContextMark.class);
    assertEquals(setOf(Dependency.RR), ipda.getDependencies(marks.get(0), marks.get(0)));
    assertEquals(setOf(Dependency.RR), ipda.getDependencies(marks.get(1), marks.get(0)));
    assertEquals(setOf(Dependency.RR), ipda.getDependencies(marks.get(1), marks.get(1)));
    List<Node> nodes = NodeFinders.findUnifyingNodes(anf, parser.parse("(fact ?)"));
    for (Node node : nodes)
    {
      if (node instanceof Application)
      {
        Application a = (Application) node;
        assertTrue(ipda.monovariantReads(a).contains(new Sym("n")));
        assertFalse(ipda.monovariantWrites(a).contains(new Sym("n")));
      }
    }
  }
  
  public void testFib()
  {
    String source = "(letrec ((fib (lambda (n)  (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))))) (fib 0))";
    Node anf = convertToAnf(source);
    Ipda ipda = doAnalysis(anf, 1);
    //assertEquals(setOf(0), ipda.getAnswer());
    source = "(letrec ((fib (lambda (n)  (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))))) (fib 1))";
    anf = convertToAnf(source);
    ipda = doAnalysis(anf, 1);
    //assertEquals(setOf(1), ipda.getAnswer());
    source = "(letrec ((fib (lambda (n)  (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))))) (fib 2))";
    anf = convertToAnf(source);
    ipda = doAnalysis(anf, 1);
    //assertEquals(setOf(1), ipda.getAnswer());
  }
  
  public void testBegin()
  {
    String source = "(let ((z 0)) (let ((writez (lambda () (set! z 123)))) (let ((readz (lambda () z))) (begin (readz) (writez)))))";
    Node anf = convertToAnf(source);
    Ipda ipda = doAnalysis(anf, 1);
    assertTrue(writes(ipda, "(lambda () (set! z 123))", "z"));
    assertFalse(reads(ipda, "(lambda () (set! z 123))", "z"));
    assertTrue(reads(ipda, "(lambda () z)", "z"));
    assertFalse(writes(ipda, "(lambda () z)", "z"));
  }
  
  public void testSetCar()
  {
    String source = "(let ((c (cons 'a 'b))) (let ((writec (lambda () (set-car! c 123)))) (writec)))";
    Node anf = convertToAnf(source);
    Ipda ipda = doAnalysis(anf, 1);
    assertTrue(writes(ipda, "(lambda () . ?)", "c"));
    assertTrue(reads(ipda, "(lambda () . ?)", "c"));
    Application a = (Application) NodeFinders.findUnifyingNode(anf, parser.parse("(writec)"));
    assertTrue(ipda.monovariantReads(a).contains(new Sym("c")));
    assertTrue(ipda.monovariantWrites(a).contains(new Sym("c")));
  }

  public void testSetCdr()
  {
    String source = "(let ((c (cons 'a 'b))) (let ((writec (lambda () (set-cdr! c 123)))) (writec)))";
    Node anf = convertToAnf(source);
    Ipda ipda = doAnalysis(anf, 1);
    assertTrue(writes(ipda, "(lambda () . ?)", "c"));
    assertTrue(reads(ipda, "(lambda () . ?)", "c"));
    Application a = (Application) NodeFinders.findUnifyingNode(anf, parser.parse("(writec)"));
    assertTrue(ipda.monovariantReads(a).contains(new Sym("c")));
    assertTrue(ipda.monovariantWrites(a).contains(new Sym("c")));
  }

}
