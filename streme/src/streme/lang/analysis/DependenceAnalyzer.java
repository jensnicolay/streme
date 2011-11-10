package streme.lang.analysis;

import java.util.HashSet;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import streme.lang.Logging;
import streme.lang.ast.Application;
import streme.lang.ast.AstDataCompiler;
import streme.lang.ast.Node;
import streme.lang.ast.Node.Type;
import streme.lang.ast.Var;
import streme.lang.ast.impl.DirectReadWriteAnalysis;
import streme.lang.ast.impl.DirectReadWriteAnalyzer;
import streme.lang.ast.impl.Printer;
import streme.lang.ast.impl.StremeDataCompiler;
import streme.lang.data.Parser2;
import streme.lang.eval.nd.NdAnalysisStreme;

public class DependenceAnalyzer
{
 
  private static final Logger LOGGER = Logger.getLogger("dependency");
  
  private VarPointerAnalysis varPointerAnalysis;
  private IpdAnalysis ipdAnalysis;
  
  public DependenceAnalyzer(VarPointerAnalysis varPointerAnalysis, IpdAnalysis ipdAnalysis)
  {
    super();
    this.varPointerAnalysis = varPointerAnalysis;
    this.ipdAnalysis = ipdAnalysis;
  }

  private static <T> Set<T> intersection(Set<T> s1, Set<T> s2)
  {
    Set<T> result = new HashSet<T>(s1);
    result.retainAll(s2);
    return result;
  }
  
  public Set<Dependency> computeLexicalDependencies(Node e1, Node e2)
  {
    DirectReadWriteAnalyzer drwa1 = new DirectReadWriteAnalyzer(varPointerAnalysis, false);
    DirectReadWriteAnalysis a1 = drwa1.analyze(e1);
    DirectReadWriteAnalysis a2 = drwa1.analyze(e2);
    Set<Var> dw1 = a1.getWrites();
    Set<Var> dw2 = a2.getWrites();
    Set<Var> dr1 = a1.getReads();
    Set<Var> dr2 = a2.getReads();
    Set<Var> rw = intersection(dr1, dw2);
    Set<Dependency> dependencies = new HashSet<Dependency>();
    if (!rw.isEmpty())
    {
      LOGGER.fine("RW direct dependency " + e1 + " -> " + e2 + " (" + rw + ")");
      dependencies.add(new Dependency<Set<Var>>(Dependency.Type.RW, Dependency.Source.LEX, rw));
    }
    Set<Var> wr = intersection(dw1, dr2);
    if (!wr.isEmpty())
    {
      LOGGER.fine("WR direct dependency " + e1 + " -> " + e2 + " (" + wr + ")");
      dependencies.add(new Dependency<Set<Var>>(Dependency.Type.WR, Dependency.Source.LEX, wr));
    }
    Set<Var> ww = intersection(dw1, dw2);
    if (!ww.isEmpty())
    {
      LOGGER.fine("WW direct dependency " + e1 + " -> " + e2 + " (" + ww + ")");
      dependencies.add(new Dependency<Set<Var>>(Dependency.Type.WW, Dependency.Source.LEX, ww));
    }
    return dependencies;
  }

  public Set<Dependency<Var>> computeVarDependencies(Var var, Node e)
  {
    DirectReadWriteAnalyzer rwa = new DirectReadWriteAnalyzer(varPointerAnalysis, true);
    DirectReadWriteAnalysis a = rwa.analyze(e);
    Set<Var> readScope2 = a.getReads();
    Set<Dependency<Var>> dependencies = new HashSet<Dependency<Var>>();
    if (readScope2.contains(var))
    {
      LOGGER.fine("read var dependency " + var + "->" + e + " (" + readScope2 + ")");
      dependencies.add(new Dependency<Var>(Dependency.Type.R, Dependency.Source.VAR, var));
    }
    Set<Var> writeScope2 = a.getWrites();
    if (writeScope2.contains(var))
    {
      LOGGER.fine("write var dependency " + var + "->" + e + " (" + writeScope2 + ")");
      dependencies.add(new Dependency<Var>(Dependency.Type.W, Dependency.Source.VAR, var));
    }
    return dependencies;
  }
  

  public Set<Dependency> computeIpDependencies(Node e1, Node e2)
  {
    IpdReadWriteAnalyzer ipdarwa1 = new IpdReadWriteAnalyzer(ipdAnalysis);
    IpdReadWriteAnalysis a1 = ipdarwa1.analyze(e1);
    IpdReadWriteAnalysis a2 = ipdarwa1.analyze(e2);
    Set<AbstractVar<Time>> r1 = a1.getReads();
    Set<AbstractVar<Time>> r2 = a2.getReads();
    Set<AbstractVar<Time>> w1 = a1.getWrites();
    Set<AbstractVar<Time>> w2 = a2.getWrites();
    Set<AbstractVar<Time>> rw = intersection(r1, w2);
    Set<Dependency> dependencies = new HashSet<Dependency>();
    if (!rw.isEmpty())
    {
      LOGGER.fine("RW ipda dependency " + e1 + " -> " + e2 + " (" + rw + ")");
      dependencies.add(new Dependency(Dependency.Type.RW, Dependency.Source.IP, rw));
    }
    Set<AbstractVar<Time>> wr = intersection(w1, r2);
    if (!wr.isEmpty())
    {
      LOGGER.fine("WR ipda dependency " + e1 + " -> " + e2 + " (" + wr + ")");
      dependencies.add(new Dependency(Dependency.Type.WR, Dependency.Source.IP, wr));
    }
    Set<AbstractVar<Time>> ww = intersection(w1, w2);
    if (!ww.isEmpty())
    {
      LOGGER.fine("WW ipda dependency " + e1 + " -> " + e2 + " (" + ww + ")");
      dependencies.add(new Dependency(Dependency.Type.WW, Dependency.Source.IP, ww));
    }
    return dependencies;
  }
  
  public Set<Dependency> computeDependencies(Node n1, Node n2)
  {
    Set<Dependency> dependencies = computeLexicalDependencies(n1, n2);
    dependencies.addAll(computeIpDependencies(n1, n2));
    if (n1.type() == Type.VAR)
    {
      dependencies.addAll(computeVarDependencies((Var) n1, n2));
    }
    if (n2.type() == Type.VAR)
    {
      dependencies.addAll(computeVarDependencies((Var) n2, n1));
    }
    return dependencies;
  }
  
  public static void main(String[] args)
  {
    Logging.setup(Level.FINE);
    String source = "(begin (define appender (lambda (h a b) (append (h a) (h b)))) (define lister (lambda (g) (lambda (x) (list (g x))))) (define square (lambda (x) (* x x))) (appender (lister square) 42 43))";
    Parser2 parser = new Parser2();
    Object data = parser.parse(source);
    AstDataCompiler dataCompiler = new StremeDataCompiler();
    Node ast = dataCompiler.compile(data);
    System.out.println("ast: " + Printer.print(ast));
    ParentAnalysis parentAnalysis = new ParentAnalyzer().analyze(ast);
    VarPointerAnalysis varPointerAnalysis = new VarPointerAnalyzer(parentAnalysis).analyze(ast);
    IpdAnalysis ipdAnalysis = new IpdAnalyzer(12, true, varPointerAnalysis).analyze(ast);
    DependenceAnalyzer dependenceAnalyzer = new DependenceAnalyzer(varPointerAnalysis, ipdAnalysis);
    NdAnalysisStreme querier = new NdAnalysisStreme(ast);
    Application app = (Application) querier.queryNode("(an-application-with-operator (a-ref-with-name 'append))");
    System.out.println(dependenceAnalyzer.computeDependencies(app.getOperands()[0], app.getOperands()[1]));
  }

}