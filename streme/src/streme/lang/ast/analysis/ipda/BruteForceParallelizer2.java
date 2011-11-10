package streme.lang.ast.analysis.ipda;

import java.io.BufferedReader;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.concurrent.ExecutorService;
import java.util.logging.Level;
import java.util.logging.Logger;

import jsr166y.ForkJoinPool;

import org.jgrapht.DirectedGraph;
import org.jgrapht.graph.SimpleDirectedGraph;

import streme.lang.Logging;
import streme.lang.StremeException;
import streme.lang.ast.Application;
import streme.lang.ast.AstDataCompiler;
import streme.lang.ast.Binding;
import streme.lang.ast.If;
import streme.lang.ast.Lambda;
import streme.lang.ast.Let;
import streme.lang.ast.Literal;
import streme.lang.ast.Node;
import streme.lang.ast.Node.Type;
import streme.lang.ast.Ref;
import streme.lang.ast.SetVar;
import streme.lang.ast.Var;
import streme.lang.ast.analysis.AnfConverter;
import streme.lang.ast.analysis.GraphUtils;
import streme.lang.ast.analysis.ReadWriteAnalyzer;
import streme.lang.ast.analysis.RenamingStrategy;
import streme.lang.ast.analysis.Undefiner;
import streme.lang.ast.analysis.ipda.DependencyGraphParallelizer.Edge;
import streme.lang.ast.analysis.ipda.DependencyGraphParallelizer.Vertex;
import streme.lang.ast.impl.AlphaConverter;
import streme.lang.ast.impl.LexicalSimplifiers;
import streme.lang.ast.impl.NodeCounter;
import streme.lang.ast.impl.ParallelConstructsRewriter;
import streme.lang.ast.impl.Printer;
import streme.lang.ast.impl.RecursiveDescentAstRewriter;
import streme.lang.ast.impl.StremeDataCompiler;
import streme.lang.ast.impl.SyntaxSimplifier;
import streme.lang.data.Parser2;
import streme.lang.data.Sym;
import streme.lang.eval.MacroExpander;
import streme.lang.eval.tanfe.AstEvaluators;
import streme.lang.eval.tanfe.TanfEvaluator;
import streme.lang.eval.tanfe.TanfStreme;

public class BruteForceParallelizer2 extends RecursiveDescentAstRewriter
{
  private static final Logger LOGGER = Logger.getLogger("bruteForce2");
  private Ipda ipda;
  private RenamingStrategy renamer;
  private DependencyGraphParallelizer dgp;
  private boolean onlyApplications;

  public BruteForceParallelizer2(Ipda ipda)
  {
    this(ipda, RenamingStrategy.NUMBER_RENAMING_STRATEGY);
  }

  public BruteForceParallelizer2(Ipda ipda, RenamingStrategy renamer)
  {
    super();
    this.ipda = ipda;
    this.renamer = renamer;
    dgp = new DependencyGraphParallelizer();
  }

  public void setOnlyApplications(boolean onlyApplications)
  {
    this.onlyApplications = onlyApplications;
  }

  protected Node rewriteLet(final Let original)
  {
    if (original.isSimpleLet())
    {
      List<Let> lets = new ArrayList<Let>();
      lets.add(original);
      Node body = original.getBody();
      while (body.type() == Type.LET)
      {
        Let blet = (Let) body;
        if (blet.isSimpleLet())
        {
          lets.add(blet);
          body = blet.getBody();
        }
        else
        {
          break;
        }
      }
//      Sym bodyName = renamer.rename(Sym.get("body"));
//      lets.add(new Let(Let.Kind.LET, new Var(bodyName), body, new Ref(bodyName)));
      if (lets.size() > 1)
//        if (lets.size() > 2)
      {
        //Node rewritten = rewriteLets(lets, new Ref(bodyName));
        Node rewritten = rewriteLets(lets, body);
        if (rewritten != null)
        {
          return rewritten;
        }
      }
    }
    return new Let(original.getKind(), rewriteBindings(original.getBindings()), rewrite(original.getBody()));
  }

  private static int gCounter = 0;

  private Node rewriteLets(List<Let> lets, Node body)
  {
    DirectedGraph<Node, Edge> initialDependencyGraph = createDependencyGraph(lets, body);
    LOGGER.finer("initial dependency graph: " + GraphUtils.dumpGraph(initialDependencyGraph));
    DirectedGraph<Vertex, Edge> g = dgp.verticize(initialDependencyGraph);
    String dotName = System.getProperty("bfpDotName");
    if (dotName != null)
    {
      dotName += "-" + gCounter++;
      dumpDot(dotName + "-pre", g);
    }
    dgp.prune(g);
    dgp.spiderCompact(g);
    LOGGER.fine("after pruning and compacting: " + GraphUtils.dumpGraph(g));
    if (onlyApplications)
    {
      List<List<Vertex>> topoSort = dgp.toplogicalLevelSort(g);
      for (List<Vertex> layer : topoSort)
      {
        if (layer.size() > 1)
        {
          Vertex nonApps = null;
          for (Vertex v : layer)
          {
            boolean containsApplication = false;
            scanForApp: for (Object o : v)
            {
              if (o instanceof Binding && ((Binding) o).getValue().type() == Type.APPLICATION)
              {
                Application application = (Application) ((Binding) o).getValue();
                if (application.getOperator().type() == Type.LAMBDA)
                {
                  containsApplication = true;
                  break scanForApp;
                }
                Ref ref = (Ref) application.getOperator();
                Sym name = ref.getName();
                Set<Object> flowsToOperator = ipda.flowsTo(name);
                if (flowsToOperator == null)
                {
                  // assume primitive: no parallelization
                }
                else
                {
                  for (Object f : flowsToOperator)
                  {
                    if (f instanceof Lambda)
                    {
                      containsApplication = true;
                      break scanForApp;
                    }
                  }
                }
              }
            }
            if (!containsApplication)
            {
              if (nonApps == null)
              {
                nonApps = v;
              }
              else
              {
                nonApps.addAll(v.getValues());
                for (Edge in : g.incomingEdgesOf(v))
                {
                  g.addEdge(g.getEdgeSource(in), nonApps);
                }
                for (Edge out : g.outgoingEdgesOf(v))
                {
                  g.addEdge(nonApps, g.getEdgeTarget(out));
                }
                g.removeVertex(v);
              }
            }
          }
        }
      }
      dgp.spiderCompact(g);
      LOGGER.fine("after collapsing non-application branches and compacting: " + GraphUtils.dumpGraph(g));
    }
    if (dotName != null)
    {
      dumpDot(dotName + "-post", g);
    }
    if (g.vertexSet().size() == 1)
    {
      LOGGER.info("cannot parallelize " + lets + " with body " + body);
      return null;
    }
    List<List<Vertex>> topologicalLevels = dgp.toplogicalLevelSort(g);
    return rewriteVertexLayer(g, topologicalLevels);
  }

  private Node rewriteVertexLayer(DirectedGraph<Vertex, Edge> g, List<List<Vertex>> vertexLayers)
  {
    List<Vertex> vertexLayer = vertexLayers.remove(0);
    LOGGER.fine("vertex layer " + vertexLayer);
    List<Binding> internalLayerBindings = new ArrayList<Binding>();
    List<Binding> parLayerBindings = new ArrayList<Binding>();
    Node body = null;
    for (int i = 0; i < vertexLayer.size(); i++)
    {
      Vertex v = vertexLayer.get(i);
      List<Binding> nodeBindings = new ArrayList<Binding>();
      for (Object o : v.getValues())
      {
        if (o instanceof Binding)
        {
          nodeBindings.add(rewriteBinding((Binding) o));
          //nodeBindings.add((Binding) o);
        }
        else
        {
          body = rewrite((Node) o);
          //body = (Node) o;
        }
      }
      if (body != null)
      {
        return new Let(Let.Kind.LETSTAR, nodeBindings, body);
      }
      List<Binding> internalNodeBindings = nodeBindings.subList(0, nodeBindings.size() - 1);
      Binding nodeBodyBinding = nodeBindings.get(nodeBindings.size() - 1);
      List<Node> nodeBody = new ArrayList<Node>();
      for (Binding internalNodeBinding : internalNodeBindings)
      {
        internalLayerBindings.add(new Binding(new Var(internalNodeBinding.getVar().getName()), Literal.UNDEFINED));
        nodeBody.add(new SetVar(new Var(internalNodeBinding.getVar().getName()), internalNodeBinding.getValue()));
      }
      nodeBody.add(nodeBodyBinding.getValue());
      parLayerBindings.add(new Binding(new Var(nodeBodyBinding.getVar().getName()), anfBegin(nodeBody)));
    }
    if (internalLayerBindings.isEmpty())
    {
      return new Let(vertexLayer.size() == 1 ? Let.Kind.LET : Let.Kind.LETPAR, parLayerBindings, rewriteVertexLayer(g,
          vertexLayers));
    }
    else
    {
      return new Let(Let.Kind.LET, internalLayerBindings, new Let(vertexLayer.size() == 1 ? Let.Kind.LET
          : Let.Kind.LETPAR, parLayerBindings, rewriteVertexLayer(g, vertexLayers)));
    }
  }

  private Node anfBegin(List<Node> nodeBody)
  {
    int numBindings = nodeBody.size();
    if (numBindings == 1)
    {
      return nodeBody.get(0);
    }
    Let let = new Let(Let.Kind.LET, new Var(renamer.rename(new Sym("$"))), nodeBody.get(numBindings - 2),
        nodeBody.get(numBindings - 1));
    for (int i = numBindings - 3; i > -1; i--)
    {
      let = new Let(Let.Kind.LET, new Var(renamer.rename(new Sym("$"))), nodeBody.get(i), let);
    }
    return let;
  }

  private <T> Set<T> intersection(Set<T> s1, Set<T> s2)
  {
    Set<T> result = new HashSet<T>(s1);
    result.retainAll(s2);
    return result;
  }

  private DirectedGraph<Node, Edge> createDependencyGraph(List<Let> lets, Node body)
  {
    LOGGER.fine("\n*** creating dependency graph for lets " + lets);
    DirectedGraph<Node, Edge> g = new SimpleDirectedGraph<Node, Edge>(Edge.class);
    for (Let let : lets)
    {
      Binding binding = let.getBindings()[0];
      g.addVertex(binding);
      LOGGER.fine(binding.getTag() + ": " + binding);
    }
    g.addVertex(body);
    LOGGER.fine(body.getTag() + ": " + body);
    for (int i = 0; i < lets.size(); i++)
    {
      Binding b1 = lets.get(i).getBindings()[0];
      Node n1 = b1.getValue();
      for (int j = i + 1; j < lets.size(); j++)
      {
        Binding b2 = lets.get(j).getBindings()[0];
        Node n2 = b2.getValue();
        // binding expression -> binding expression constraints
        Set<Sym> r1 = getReads(ipda, n1);
        Set<Sym> r2 = getReads(ipda, n2);
        Set<Sym> w1 = getWrites(ipda, n1);
        Set<Sym> w2 = getWrites(ipda, n2);
        Set<Sym> rr = intersection(r1, r2);
        if (!rr.isEmpty())
        {
          LOGGER.info("RR dependency " + b1.getTag() + ", " + b2.getTag() + " (" + rr + ")");
        }
        Set<Sym> rw = intersection(r1, w2);
        if (!rw.isEmpty())
        {
          Edge e = g.addEdge(b1, b2);
          // e.setProperty("type", "rw");
          LOGGER.fine("RW dependency " + b1.getTag() + " -> " + b2.getTag() + " (" + rw + ")");
        }
        Set<Sym> wr = intersection(w1, r2);
        if (!wr.isEmpty())
        {
          Edge e = g.addEdge(b1, b2);
          // e.setProperty("type", "wr");
          LOGGER.fine("WR dependency " + b1.getTag() + " -> " + b2.getTag() + " (" + wr + ")");
        }
        Set<Sym> ww = intersection(w1, w2);
        if (!ww.isEmpty())
        {
          Edge e = g.addEdge(b1, b2);
          // e.setProperty("type", "ww");
          LOGGER.fine("WW dependency " + b1.getTag() + " -> " + b2.getTag() + " (" + ww + ")");
        }
        // binding variable -> binding expression constraints
        ReadWriteAnalyzer rwa = new ReadWriteAnalyzer(true);
        n2.accept(rwa);
        List<Sym> readScope2 = rwa.getReads();
        if (readScope2.contains(b1.getVar().getName()))
        {
          g.addEdge(b1, b2);
          LOGGER.fine("read scope dependency " + b1.getTag() + "->" + b2.getTag() + " (" + readScope2 + ")");
        }
        List<Sym> writeScope2 = rwa.getWrites();
        if (writeScope2.contains(b1.getVar().getName()))
        {
          g.addEdge(b1, b2);
          LOGGER.fine("write scope dependency " + b1.getTag() + "->" + b2.getTag() + " (" + writeScope2 + ")");
        }
      }
    }
    // connect sink
    List<Node> sinks = GraphUtils.getSinks(g);
    sinks.remove(body);
    for (Node v : sinks)
    {
      g.addEdge(v, body);
      LOGGER.fine("connecting sink " + v.getTag() + "->" + body.getTag());
    }
    return g;
  }

  private Set<Sym> getReads(Ipda ipda, Node n)
  {
    Set<Sym> ir = getIpdaReadDependencies(ipda, n);
    if (!ir.isEmpty())
    {
      LOGGER.finer("ipda R dependency " + n + " (" + ir + ")");
    }
    Set<Sym> dr = getDirectReadDependencies(n);
    if (!dr.isEmpty())
    {
      LOGGER.finer("direct R dependency " + n + " (" + dr + ")");
    }
    Set<Sym> r = new HashSet<Sym>(ir);
    r.addAll(dr);
    return r;
  }

  private Set<Sym> getWrites(Ipda ipda, Node n)
  {
    Set<Sym> iw = getIpdaWriteDependencies(ipda, n);
    if (!iw.isEmpty())
    {
      LOGGER.finer("ipda W dependency " + n + " (" + iw + ")");
    }
    Set<Sym> dw = getDirectWriteDependencies(n);
    if (!dw.isEmpty())
    {
      LOGGER.finer("direct W dependency " + n + " (" + dw + ")");
    }
    Set<Sym> w = new HashSet<Sym>(iw);
    w.addAll(dw);
    return w;
  }

  private Set<Sym> getIpdaReadDependencies(Ipda ipda, Node n)
  {
    switch (n.type())
    {
      case APPLICATION:
        Application application = (Application) n;
        return ipda.monovariantReads(application);
      case IF:
      {
        If ff = (If) n;
        Set<Sym> r = new HashSet<Sym>();
        r.addAll(getIpdaReadDependencies(ipda, ff.getConsequent()));
        r.addAll(getIpdaReadDependencies(ipda, ff.getAlternate()));
        return r;
      }
      case LET:
      {
        Let let = (Let) n;
        Set<Sym> r = new HashSet<Sym>();
        r.addAll(getIpdaReadDependencies(ipda, let.getValue(0)));
        r.addAll(getIpdaReadDependencies(ipda, let.getBody()));
        return r;
      }
      case SETVAR:
      case REF:
      case LAMBDA:
      case LITERAL:
        return new HashSet<Sym>();
      default:
        throw new StremeException("cannot handle " + n);
    }
  }

  private Set<Sym> getIpdaWriteDependencies(Ipda ipda, Node n)
  {
    switch (n.type())
    {
      case APPLICATION:
        Application application = (Application) n;
        return ipda.monovariantWrites(application);
      case IF:
      {
        If ff = (If) n;
        Set<Sym> w = new HashSet<Sym>();
        w.addAll(getIpdaWriteDependencies(ipda, ff.getConsequent()));
        w.addAll(getIpdaWriteDependencies(ipda, ff.getAlternate()));
        return w;
      }
      case LET:
      {
        Let let = (Let) n;
        Set<Sym> w = new HashSet<Sym>();
        w.addAll(getIpdaReadDependencies(ipda, let.getValue(0)));
        w.addAll(getIpdaReadDependencies(ipda, let.getBody()));
        return w;
      }
      case SETVAR:
      case REF:
      case LITERAL:
      case LAMBDA:
        return new HashSet<Sym>();
      default:
        throw new StremeException("cannot handle " + n);
    }
  }

  private Set<Sym> getDirectReadDependencies(Node n)
  {
    ReadWriteAnalyzer rwa = new ReadWriteAnalyzer(false);
    n.accept(rwa);
    return new HashSet<Sym>(rwa.getReads());
  }

  private Set<Sym> getDirectWriteDependencies(Node n)
  {
    ReadWriteAnalyzer rwa = new ReadWriteAnalyzer(false);
    n.accept(rwa);
    return new HashSet<Sym>(rwa.getWrites());
  }

  /** for testing purposes */
  private static String load(String name)
  {
    String fileName = "src/test/" + name;
    try
    {
      BufferedReader reader = new BufferedReader(new FileReader(fileName));
      StringBuilder b = new StringBuilder();
      String s;
      while ((s = reader.readLine()) != null)
      {
        b.append(s).append("\n");
      }
      reader.close();
      return b.toString();
    }
    catch (Exception e)
    {
      throw new StremeException("cannot load " + fileName, e);
    }
  }

  public static void dumpDot(String name, DirectedGraph<Vertex, Edge> g)
  {
//    try
//    {
//      Writer sw = new StringWriter();
//      PrintWriter pw = new PrintWriter(sw, true);
//      pw.println("digraph G {");
//      for (Vertex v : g.vertexSet())
//      {
//        List<Object> values = v.getValues();
//        if (!(values.get(values.size() - 1) instanceof Binding))
//        {
//          pw.println(nodeString(v) + " [shape=box]");
//        }
//        for (Edge e : g.outgoingEdgesOf(v))
//        {
//          pw.println(nodeString(v) + " -> " + nodeString(g.getEdgeTarget(e)));
//        }
//      }
//      pw.println("}");
//      pw.close();
//      sw.close();
//      Process process = Runtime.getRuntime().exec("dot.exe -Tps2");
//      PrintWriter writer = new PrintWriter(process.getOutputStream(), true);
//      writer.println(sw.toString());
//      process.getOutputStream().write(26);
//      process.getOutputStream().flush();
//      BufferedReader reader = new BufferedReader(new InputStreamReader(process.getInputStream()));
//      PrintWriter fw = new PrintWriter(name + ".eps");
//      String line;
//      while ((line = reader.readLine()) != null)
//      {
//        fw.println(line);
//      }
//      fw.close();
//      BufferedReader error = new BufferedReader(new InputStreamReader(process.getErrorStream()));
//      while ((line = error.readLine()) != null)
//      {
//        System.err.println(line);
//      }
//      Writer dotWriter = new FileWriter(name + ".dot");
//      dotWriter.write(sw.toString());
//      dotWriter.close();
//    }
//    catch (Exception e)
//    {
//      e.printStackTrace();
//    }
  }

  private static String nodeString(Vertex v)
  {
    // return String.valueOf(v).replace('[', '{').replace(']', '}');
    StringBuilder sb = new StringBuilder();
    List<Object> values = v.getValues();
    Iterator<Object> iter = values.iterator();
    sb.append(vertexValString(iter.next()));
    while (iter.hasNext())
    {
      sb.append(", ").append(vertexValString(iter.next()));
    }
    return "\"" + sb + "\"";
  }

  private static String vertexValString(Object next)
  {
    if (next instanceof Binding)
    {
      Binding b = (Binding) next;
      return b.getVar().toString();
    }
    return "=" + next.toString();
  }

  public static void main(String[] args)
  {
    // String source = "(letrec ((fact (lambda (n) (if (zero? n) 1 (* n (fact (- n 1))))))) (fact 20))";
    // String source = "(let* ((z 0) (writez (lambda () (set! z 123))) (readz (lambda () z))) (cons (writez) (readz)))";
    // String source =
    // "(let ((z 0)) (let ((writez (lambda () (set! z 123)))) (let ((readz (lambda () z))) (begin (writez) (readz)))))";
    // final int NUM_ITER = 1;
    String source = "(letrec ((fib (lambda (n) (if (< n 2) n (+ (fib (- n 2)) (fib (- n 1))))))) (fib 30))";
    // String source =
    // "(let* ((z '()) (appender (lambda (f a b) (append (f a) (f b)))) (conser (lambda (x) (set! z (cons x z)) z))) (appender conser 1 2))";
    // String source =
    // "(let* ((z '()) (appender (lambda (f a b) (append (f a) (f b)))) (square (lambda (x) (list (* x x))))) (appender square 1 2))";
    // String source = load("gabriel/tak.str"); final int NUM_ITER = 1;
    // String source = load("gabriel/cpstak.str"); final int NUM_ITER = 10;
    // String source = load("other/mbrotcore.str"); final int NUM_ITER = 10;
    // String source = load("gabriel/boyer.str"); final int NUM_ITER = 1;
    // String source = load("gabriel/nboyer.str"); final int NUM_ITER = 1;
    // String source = load("gabriel/diviter.str"); final int NUM_ITER = 1000;
    // String source = load("gabriel/divrec.str");final int NUM_ITER = 1000;
    // String source = load("gabriel/pnpoly.str");final int NUM_ITER = 1000;
    // String source = load("gabriel/browse.str"); final int NUM_ITER = 1;
    // String source = load("gabriel/triangl.str"); final int NUM_ITER = 1;
    // String source = load("other/ack.str"); final int NUM_ITER = 1;
    // String source = load("other/graphs.str"); final int NUM_ITER = 10;
    // String source = load("other/mazefun.str"); final int NUM_ITER = 1;
    // String source = load("other/factor.str"); final int NUM_ITER = 20;
    // String source = load("other/nqueens.str"); final int NUM_ITER = 1;
    // String source = load("other/mergesort.str");
    final int NUM_ITER = 1;
    // String source = load("other/quicksort.str"); final int NUM_ITER = 1;
    // String source =
    // "(let ((z 0)) (let ((writez (lambda () (set! z 123)))) (let ((readz (lambda () z))) (begin (writez) (readz)))))";
    // String source = "(let* ((z 0) (readz (lambda () z))) (cons (readz) (readz)))";
    // String source =
    // "(let* ((z 0) (s (+ z 2)) (x (+ z 5)) (y (+ z 7)) (u (+ s x y)) (u1 (+ u 11)) (u2 (+ u 13)) (r (* x y))) r)";
    // String source="(let ((l '())) (letrec ((f (lambda (i) (if (zero? i) i (set! l 123))))) (f 10) l))";
    // String
    // source="(let ((l '())) (letrec ((f (lambda (i) (if (zero? i) i (begin (set! l (cons i l)) (f (- i 1))))))) (f 10) (f 10) l)))";
    //String source = load("other/ho2.str");
    Logging.setup(Level.FINE);
    Parser2 parser = new Parser2();
    Object data = parser.parse(source);
    MacroExpander expander = new MacroExpander();
    expander.setLetToLambda(false);
    expander.setLetrecToLambda(false);
    Object expanded = expander.rewrite(data);
    AstDataCompiler dataCompiler = new StremeDataCompiler();
    Node ast = Undefiner.undefine(dataCompiler.compile(expanded));
    System.out.println("ast: " + ast);
    AlphaConverter alphaConverter = new AlphaConverter(RenamingStrategy.NUMBER_RENAMING_STRATEGY);
    Node alphaAst = alphaConverter.rewrite(ast);
    AnfConverter anfConverter = new AnfConverter(RenamingStrategy.NUMBER_RENAMING_STRATEGY);
    Node anf = anfConverter.rewrite(alphaAst);
    System.out.println("anf: " + Printer.print(anf));
    Ipda ipda = new Ipda(3);
    ipda.analyze(anf);
    BruteForceParallelizer2 bfp = new BruteForceParallelizer2(ipda);
    Node lspast = bfp.rewrite(anf);
    Node past = SyntaxSimplifier.simplify(lspast);
    System.out.println("past: " + past);
    Node corePast = new ParallelConstructsRewriter().rewrite(past);
    corePast = SyntaxSimplifier.simplify(corePast);
    System.out.println("corePast: " + corePast);
    Node corePanf = LexicalSimplifiers.simplify(SyntaxSimplifier.simplify(anfConverter.rewrite(corePast)));
    System.out.println("corePanf: " + corePanf);
    ExecutorService executor = new ForkJoinPool();
    TanfEvaluator evaluator = new TanfEvaluator(executor);
    TanfStreme streme = new TanfStreme(executor);
    Logging.setup(Level.OFF);
    Object[] anfRet = new Object[NUM_ITER];
    long anfStart = System.currentTimeMillis();
    for (int i = 0; i < NUM_ITER; i++)
    {
      anfRet[i] = AstEvaluators.evaluate(evaluator, anf, streme.globalEnv());
    }
    long anfEnd = System.currentTimeMillis();
    System.out.println(Arrays.toString(anfRet));
    long anfTime = anfEnd - anfStart;
    System.out.println(anfTime);
    Object[] panfRet = new Object[NUM_ITER];
    long panfStart = System.currentTimeMillis();
    for (int i = 0; i < NUM_ITER; i++)
    {
      panfRet[i] = AstEvaluators.evaluate(evaluator, corePanf, streme.globalEnv());
    }
    long panfEnd = System.currentTimeMillis();
    System.out.println(Arrays.toString(panfRet));
    long panfTime = panfEnd - panfStart;
    System.out.println(panfTime);
    NodeCounter nc = new NodeCounter();
    Object touchPattern = parser.parse("(touch ?)");
    nc.countPattern(touchPattern);
    corePanf.accept(nc);
    System.out.println("node count: total=" + nc.getTotalCount() + " futures=" + nc.getTypeCount(Node.Type.FUTURE)
        + " touches=" + nc.getPatternCount(touchPattern));
    System.out.println("anf " + anfTime + " corePanf " + panfTime);
    executor.shutdown();
  }
}