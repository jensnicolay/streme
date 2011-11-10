package streme.lang.test;

import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import junit.framework.TestCase;
import streme.lang.DirectExecutor;
import streme.lang.analysis.AbstractVar;
import streme.lang.analysis.Closure;
import streme.lang.analysis.IpdAnalysis;
import streme.lang.analysis.IpdAnalyzer;
import streme.lang.analysis.ParentAnalysis;
import streme.lang.analysis.ParentAnalyzer;
import streme.lang.analysis.State;
import streme.lang.analysis.StatefulIpdAnalyzer;
import streme.lang.analysis.Time;
import streme.lang.analysis.VarPointerAnalysis;
import streme.lang.analysis.VarPointerAnalyzer;
import streme.lang.ast.Application;
import streme.lang.ast.Define;
import streme.lang.ast.Lambda;
import streme.lang.ast.Node;
import streme.lang.ast.Var;
import streme.lang.ast.impl.NodeFinders;
import streme.lang.ast.impl.StremeDataCompiler;
import streme.lang.data.Pair;
import streme.lang.data.Parser2;
import streme.lang.data.Sym;
import streme.lang.eval.nd.NdAnalysisStreme;
import streme.lang.eval.tanfe.StremeContext;
import streme.lang.eval.tanfe.TanfStreme;

public class IpdAnalyzerTests extends TestCase
{
  private static final int K = 24;
  private Parser2 parser = new Parser2();
  private StremeDataCompiler compiler = new StremeDataCompiler();

  private Set<Lambda> lambdaify(Set<?> procs)
  {
    Set<Lambda> lambdas = new HashSet<Lambda>();
    for (Object proc : procs)
    {
      Lambda lambda = ((Closure) proc).getLambda();
      lambdas.add(lambda);
    }
    return lambdas;
  }

  private Node testMonoValues(String source, Set<Object> expected)
  {
    Node ast = createAst(source);
    IpdAnalysis ipdAnalysis = doAnalysis(ast);
    Set<State> result2 = ipdAnalysis.getResult();
    Set<Object> result3 = getMonoValues(result2);
    assertEquals(expected, result3);
    return ast;
  }

  private Set<Object> getMonoValues(Set<State> result2)
  {
    Set<Object> result3 = new HashSet<Object>();
    for (State state : result2)
    {
      result3.addAll(state.getValues());
    }
    return result3;
  }

  private IpdAnalysis doAnalysis(Node ast)
  {
    return doAnalysis(ast, K, true);
  }

  private IpdAnalysis doAnalysis(Node ast, int k, boolean gc)
  {
    ParentAnalysis parentAnalysis = new ParentAnalyzer().analyze(ast);
    VarPointerAnalysis varPointerAnalysis = new VarPointerAnalyzer(parentAnalysis).analyze(ast);
    IpdAnalysis ipdAnalysis = new IpdAnalyzer(k, gc, varPointerAnalysis).analyze(ast);
    return ipdAnalysis;
  }

  private Node createAst(String source)
  {
    Node ast = compiler.compile(parser.parse(source));
    return ast;
  }

  private Set<Object> setOf(Object... els)
  {
    Set<Object> set = new LinkedHashSet<Object>();
    for (Object el : els)
    {
      set.add(el);
    }
    return set;
  }

  public void testSimple()
  {
    testMonoValues("(begin (define f (lambda () 5)) (f))", setOf(5));
  }

  public void testDefine()
  {
    Node ast = testMonoValues("(begin (define x 10) x)", (setOf(10)));
    IpdAnalysis ipdAnalysis = doAnalysis(ast);
    NdAnalysisStreme querier = new NdAnalysisStreme(ast);
    Var var = (Var) querier.queryNode("(a-var-with-name 'x)");
    assertEquals((setOf(10)), ipdAnalysis.getValues(var));
    assertEquals((setOf(10)), ipdAnalysis.getMonoValues(var));
    querier.shutdown();
  }

  public void testDefine1()
  {
    Node ast = testMonoValues("(begin (define x 10) (set! x 11) x)", (setOf(11)));
    IpdAnalysis ipdAnalysis = doAnalysis(ast);
    NdAnalysisStreme querier = new NdAnalysisStreme(ast);
    Var var = (Var) querier.queryNode("(let ((x (a-var-with-name 'x))) (require (not (set-var? x))) x)");
    assertEquals((setOf(11)), ipdAnalysis.getValues(var));
    assertEquals((setOf(10, 11)), ipdAnalysis.getMonoValues(var));
    querier.shutdown();
  }

  public void testDefine2()
  {
    Node ast = testMonoValues("(begin (define x 10) (set! x 11) (set! x 12) x)", (setOf(12)));
    NdAnalysisStreme querier = new NdAnalysisStreme(ast);
    Var var = (Var) querier.queryNode("(let ((x (a-var-with-name 'x))) (require (not (set-var? x))) x)");
    IpdAnalysis ipdAnalysis = doAnalysis(ast);
    assertEquals((setOf(12)), ipdAnalysis.getValues(var));
    assertEquals((setOf(10, 11, 12)), ipdAnalysis.getMonoValues(var));
    querier.shutdown();
  }

  public void testLambda()
  {
    Node ast = testMonoValues("(begin (define f (lambda (x) x)) (f 123))", (setOf(123)));
    NdAnalysisStreme querier = new NdAnalysisStreme(ast);
    Var var = (Var) querier.queryNode("(a-var-with-name 'x)");
    IpdAnalysis ipdAnalysis = doAnalysis(ast);
    assertEquals((setOf(123)), ipdAnalysis.getValues(var));
    assertEquals((setOf(123)), ipdAnalysis.getMonoValues(var));
    querier.shutdown();
  }

  public void testLambda1()
  {
    Node ast = testMonoValues("(begin (define f (lambda (x) (set! x #f))) (f 123))", (setOf(Void.TYPE)));
    NdAnalysisStreme querier = new NdAnalysisStreme(ast);
    Var var = (Var) querier.queryNode("(let ((x (a-var-with-name 'x))) (require (not (set-var? x))) x)");
    IpdAnalysis ipdAnalysis = doAnalysis(ast);
    assertEquals((setOf(Boolean.FALSE)), ipdAnalysis.getValues(var));
    assertEquals((setOf(123, Boolean.FALSE)), ipdAnalysis.getMonoValues(var));
    querier.shutdown();
  }

  public void test1()
  {
    String source = "(begin (define z 123) (define f (lambda () z)) (f) (f))";
    Node ast = createAst(source);
    IpdAnalysis ipda = doAnalysis(ast);
    Set<State> result = ipda.getResult();
    assertEquals(setOf(123), getMonoValues(result));
    assertEquals(1, result.size());
    State state = result.iterator().next();
    NdAnalysisStreme querier = new NdAnalysisStreme(ast);
    List<Node> apps = querier.queryNodes("(an-application-with-operator (a-ref-with-name 'f))");
    assertEquals(new Time().tick(apps.get(0).getTag(), K).tick(apps.get(1).getTag(), K), state.getTime());
    querier.shutdown();
  }

  public void test2()
  {
    Node ast = createAst("(begin (define f (lambda () 5)) (define readz (lambda () z)) (f) (define z 123) (readz))");
    IpdAnalysis ipda = doAnalysis(ast);
    Set<State> result = ipda.getResult();
    assertEquals(setOf(123), getMonoValues(result));
    assertEquals(1, result.size());
    State state = result.iterator().next();
    NdAnalysisStreme querier = new NdAnalysisStreme(ast);
    Node fApp = querier.queryNode("(an-application-with-operator (a-ref-with-name 'f))");
    Node readzApp = querier.queryNode("(an-application-with-operator (a-ref-with-name 'readz))");
    assertEquals(new Time().tick(fApp.getTag(), K).tick(readzApp.getTag(), K), state.getTime());
  }

  public void testLetMultipleBindings()
  {
    String source = "(let ((a 1) (b 2)) (+ a b))";
    Node ast = createAst(source);
    IpdAnalysis ipda = doAnalysis(ast);
    Set<State> result = ipda.getResult();
    assertEquals(setOf(3), getMonoValues(result));
    assertEquals(1, result.size());
  }

  public void testLetEnv()
  {
    String source = "(begin (define a #t) (cons (let ((a 123)) a) a))";
    Node ast = createAst(source);
    IpdAnalysis ipda = doAnalysis(ast);
    Set<State> result = ipda.getResult();
    assertEquals(setOf(Pair.cons(123, true)), getMonoValues(result));
    assertEquals(1, result.size());
  }

  public void testLetStarEnv()
  {
    String source = "(begin (define a #t) (cons (let* ((a 123)) a) a))";
    Node ast = createAst(source);
    IpdAnalysis ipda = doAnalysis(ast);
    Set<State> result = ipda.getResult();
    assertEquals(setOf(Pair.cons(123, true)), getMonoValues(result));
    assertEquals(1, result.size());
  }

  public void testReadWrite()
  {
    Node ast = createAst("(begin (define z #f) (define f (lambda () (g))) (define g (lambda () (h))) (define h (lambda () (set! z 123))) (f) z)");
    IpdAnalysis ipda = doAnalysis(ast);
    Set<State> result = ipda.getResult();
    assertEquals(setOf(123), getMonoValues(result));
    assertEquals(1, result.size());
    NdAnalysisStreme querier = new NdAnalysisStreme(ast);
    Var z = (Var) querier.queryNode("(let ((z (a-var-with-name 'z))) (require (not (set-var? z))) z)");
    Var f = (Var) querier.queryNode("(a-var-with-name 'f)");
    Var g = (Var) querier.queryNode("(a-var-with-name 'g)");
    Var h = (Var) querier.queryNode("(a-var-with-name 'h)");
    // Lambda lf = (Lambda) NodeFinders.findUnifyingNode(ast, parser.parse("(lambda () (g))"));
    // Lambda lg = (Lambda) NodeFinders.findUnifyingNode(ast, parser.parse("(lambda () (h))"));
    // Lambda lh = (Lambda) NodeFinders.findUnifyingNode(ast, parser.parse("(lambda () (set! . ?))"));
    Application af = (Application) querier.queryNode("(an-application-with-operator (a-ref-with-name 'f))");
    Application ag = (Application) querier.queryNode("(an-application-with-operator (a-ref-with-name 'g))");
    Application ah = (Application) querier.queryNode("(an-application-with-operator (a-ref-with-name 'h))");
    assertEquals(setOf(123), ipda.getValues(z));
    assertEquals(setOf(Boolean.FALSE, 123), ipda.getMonoValues(z));
    // Map<Object, Set<AbstractProcedure<Application>>> zWriters = IpdaAnalyzer.getWriters(z);
    // assertEquals(1, zWriters.size());
    // assertEquals(3, zWriters.values().iterator().next().size());
    // Map<Object, Set<AbstractProcedure<Application>>> zReaders = IpdaAnalyzer.getReaders(z);
    // assertEquals(1, zReaders.size());
    // assertEquals(setOf(z), IpdaAnalyzer.getMonoWrites(lf));
    // assertEquals(setOf(z), IpdaAnalyzer.getMonoWrites(lg));
    // assertEquals(setOf(z), IpdaAnalyzer.getMonoWrites(lh));
    assertEquals(setOf(z), AbstractVar.monovariant(ipda.getWrites(af)));
    assertEquals(setOf(z), AbstractVar.monovariant(ipda.getWrites(ag)));
    assertEquals(setOf(z), AbstractVar.monovariant(ipda.getWrites(ah)));
    assertEquals(setOf(g, h), AbstractVar.monovariant(ipda.getReads(af)));
    assertEquals(setOf(h), AbstractVar.monovariant(ipda.getReads(ag)));
    assertEquals(setOf(), AbstractVar.monovariant(ipda.getReads(ah)));
    // assertEquals(setOf(g, h), IpdaAnalyzer.getMonoReads(lf));
    // assertEquals(setOf(h), IpdaAnalyzer.getMonoReads(lg));
    // assertEquals(setOf(), IpdaAnalyzer.getMonoReads(lh));
    // assertEquals(setOf(lf, lg, lh), lambdaify(IpdaAnalyzer.getMonoWriters(z)));
    // assertEquals(setOf(), lambdaify(IpdaAnalyzer.getMonoReaders(z)));
  }

  public void testFinite1()
  {
    Node ast = createAst("(begin (define t (lambda () (t))) (t))");
    IpdAnalysis ipda = doAnalysis(ast);
    Set<State> result = ipda.getResult();
    assertEquals(0, result.size());
  }

  public void testCounter()
  {
    String source = "(begin (define counter (lambda (c) (if (zero? c) 'boem (counter (- c 1))))) (counter 2))";
    Node ast = createAst(source);
    IpdAnalysis ipda = doAnalysis(ast);
    Set<State> result = ipda.getResult();
    assertEquals(1, result.size());
    Set<Object> monoValues = getMonoValues(result);
    assertEquals(setOf(new Sym("boem")), monoValues);
    NdAnalysisStreme querier = new NdAnalysisStreme(ast);
    Var c = (Var) querier.queryNode("(a-var-with-name 'c)");
    Var counter = (Var) querier.queryNode("(a-var-with-name 'counter)");
    // Lambda lcounter = (Lambda) NodeFinders.findUnifyingNode(ast, parser.parse("(lambda (c) . ?)"));
    Application acounter2 = (Application) NodeFinders.findUnifyingNode(ast, parser.parse("(counter 2)"));
    Application acounterrec = (Application) NodeFinders.findUnifyingNode(ast, parser.parse("(counter (- c 1))"));
    assertEquals(setOf(), AbstractVar.monovariant(ipda.getWrites(acounter2)));
    assertEquals(setOf(), AbstractVar.monovariant(ipda.getWrites(acounterrec)));
    assertEquals(setOf(c, counter), AbstractVar.monovariant(ipda.getReads(acounter2)));
    assertEquals(setOf(c, counter), AbstractVar.monovariant(ipda.getReads(acounterrec)));
    // assertEquals(setOf(lcounter), lambdaify(IpdaAnalyzer.getMonoReaders(c)));
    // assertEquals(setOf(), IpdaAnalyzer.getMonoWriters(c));
    // assertEquals(setOf(lcounter), lambdaify(IpdaAnalyzer.getMonoReaders(counter)));
    // assertEquals(setOf(), IpdaAnalyzer.getMonoWriters(counter));
  }

  public void testMorePreciseThanOldIpda()
  {
    String source = "(let ((z 0)) (let ((writez (lambda () (set! z 123)))) (let ((readz (lambda () z))) (let ((p3 (readz))) (writez)))))";
    Node ast = createAst(source);
    IpdAnalysis ipda = doAnalysis(ast);
    Set<State> result = ipda.getResult();
    assertEquals(1, result.size());
    Set<Object> monoValues = getMonoValues(result);
    assertEquals(setOf(Void.TYPE), monoValues);
    NdAnalysisStreme querier = new NdAnalysisStreme(ast);
    Var z = (Var) querier.queryNode("(let ((z (a-var-with-name 'z))) (require (not (set-var? z))) z)");
    // Var writez = getUniquelyNamedVar(ast, "writez");
    // Var readz = getUniquelyNamedVar(ast, "readz");
    // Lambda lwritez = (Lambda) NodeFinders.findUnifyingNode(ast, parser.parse("(lambda () (set! . ?))"));
    // Lambda lreadz = (Lambda) NodeFinders.findUnifyingNode(ast, parser.parse("(lambda () z)"));
    Application areadz = (Application) querier.queryNode("(an-application-with-operator (a-ref-with-name 'readz))");
    Application awritez = (Application) querier.queryNode("(an-application-with-operator (a-ref-with-name 'writez))");
    // assertEquals(setOf(lwritez), lambdaify(IpdaAnalyzer.getMonoWriters(z)));
    // assertEquals(setOf(lreadz), lambdaify(IpdaAnalyzer.getMonoReaders(z)));
    assertEquals(setOf(), AbstractVar.monovariant(ipda.getReads(awritez)));
    assertEquals(setOf(z), AbstractVar.monovariant(ipda.getReads(areadz)));
    assertEquals(setOf(z), AbstractVar.monovariant(ipda.getWrites(awritez)));
    assertEquals(setOf(), AbstractVar.monovariant(ipda.getWrites(areadz)));
    querier.shutdown();
  }

  public void testCpsy()
  {
    String source = "(let ((id (lambda (x q) (q x)))) (id 3 (lambda (v1) (id 4 (lambda (v2) (- v2))))))";
    Node ast = createAst(source);
    IpdAnalysis ipda = doAnalysis(ast);
    Set<State> result = ipda.getResult();
    assertEquals(1, result.size());
    Set<Object> monoValues = getMonoValues(result);
    assertEquals(setOf(-4), monoValues);
    Object lambda1 = NodeFinders.findUnifyingNode(ast, parser.parse("(lambda (x q) ?)"));
    Object lambda2 = NodeFinders.findUnifyingNode(ast, parser.parse("(lambda (v1) ?)"));
    Object lambda3 = NodeFinders.findUnifyingNode(ast, parser.parse("(lambda (v2) ?)"));
    NdAnalysisStreme querier = new NdAnalysisStreme(ast);
    Var id = (Var) querier.queryNode("(a-var-with-name 'id)");
    Var q = (Var) querier.queryNode("(a-var-with-name 'q)");
    Var x = (Var) querier.queryNode("(a-var-with-name 'x)");
    Var v1 = (Var) querier.queryNode("(a-var-with-name 'v1)");
    Var v2 = (Var) querier.queryNode("(a-var-with-name 'v2)");
    assertEquals(setOf(lambda1), lambdaify(ipda.getMonoValues(id)));
    assertEquals(setOf(lambda2, lambda3), lambdaify(ipda.getMonoValues(q)));
    assertEquals(setOf(3, 4), ipda.getMonoValues(x));
    assertEquals(setOf(3), ipda.getMonoValues(v1));
    assertEquals(setOf(4), ipda.getMonoValues(v2));
    ipda = doAnalysis(ast, 0, false);
    assertEquals(setOf(lambda1), lambdaify(ipda.getMonoValues(id)));
    assertEquals(setOf(lambda2, lambda3), lambdaify(ipda.getMonoValues(q)));
    assertEquals(setOf(3, 4), ipda.getMonoValues(x));
    assertEquals(setOf(3, 4), ipda.getMonoValues(v1));
    assertEquals(setOf(3, 4), ipda.getMonoValues(v2));
    querier.shutdown();
  }

  public void testFib()
  {
    String source = "(letrec ((fib (lambda (n) (if (< n 2) n (+ (fib (- n 2)) (fib (- n 1))))))) (fib 4))";
    Node ast = createAst(source);
    IpdAnalysis ipda = doAnalysis(ast);
    Set<State> result = ipda.getResult();
    assertEquals(1, result.size());
    Set<Object> monoValues = getMonoValues(result);
    assertEquals(setOf(3), monoValues);
  }

  public void testFibWithLet()
  {
    String source = "(letrec ((fib (lambda (n) (if (< n 2) n (let ((a (fib (- n 1))) (b (fib (- n 2)))) (+ a b)))))) (fib 4))";
    Node ast = createAst(source);
    IpdAnalysis ipda = doAnalysis(ast);
    Set<State> result = ipda.getResult();
    assertEquals(1, result.size());
    Set<Object> monoValues = getMonoValues(result);
    assertEquals(setOf(3), monoValues);
  }

  public void testReadWriteWithBindingTimes()
  {
    String source = "(let* ((z 0) (writez (lambda () (set! z 123))) (readz (lambda () z))) (cons (writez) (readz)))";
    Node ast = createAst(source);
    IpdAnalysis ipda = doAnalysis(ast);
    Set<State> result = ipda.getResult();
    assertEquals(1, result.size());
    Set<Object> monoValues = getMonoValues(result);
    assertEquals(setOf(Pair.cons(Void.TYPE, 123)), monoValues);
    Application awritez = (Application) NodeFinders.findUnifyingNode(ast, parser.parse("(writez)"));
    Application areadz = (Application) NodeFinders.findUnifyingNode(ast, parser.parse("(readz)"));
    NdAnalysisStreme querier = new NdAnalysisStreme(ast);
    Var z = (Var) querier.queryNode("(let ((z (a-var-with-name 'z))) (require (not (set-var? z))) z)");
    AbstractVar<Time> z_ = new AbstractVar<Time>(z, new Time());
    assertEquals(setOf(), ipda.getReads(awritez));
    assertEquals(setOf(z_), ipda.getWrites(awritez));
    assertEquals(setOf(z_), ipda.getReads(areadz));
    assertEquals(setOf(), ipda.getWrites(areadz));
  }
  
  public void testAbstractGarbageCollection()
  {
    String source = "(let* ((id (lambda (x) x)) (u (id 1))) (id 2))";
    Node ast = createAst(source);
    IpdAnalysis ipda = doAnalysis(ast, 0, false);
    Set<State> result = ipda.getResult();
    assertEquals(1, result.size());
    Set<Object> monoValues = getMonoValues(result);
    assertEquals(setOf(1, 2), monoValues);
    ipda = doAnalysis(ast, 0, true);
    result = ipda.getResult();
    assertEquals(1, result.size());
    monoValues = getMonoValues(result);
    assertEquals(setOf(2), monoValues);
  }
}
