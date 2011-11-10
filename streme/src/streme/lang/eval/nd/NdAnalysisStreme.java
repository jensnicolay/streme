package streme.lang.eval.nd;

import java.io.StringReader;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Callable;

import streme.lang.StremeException;
import streme.lang.TCont;
import streme.lang.ast.AstDataCompiler;
import streme.lang.ast.Node;
import streme.lang.ast.impl.StremeDataCompiler;
import streme.lang.data.Parser2;
import streme.lang.data.Sym;

public class NdAnalysisStreme
{
  private Parser2 parser = new Parser2();
  private AstDataCompiler compiler = new StremeDataCompiler();
  private NdStreme streme = new NdStreme();

  public NdAnalysisStreme(String source)
  {
    Node ast = compiler.compile(parser.parse(source));
    initialize(ast);
  }

  public NdAnalysisStreme(Node ast)
  {
    initialize(ast);
  }
  
  public NdStreme getStreme()
  {
    return streme;
  }

  private void initialize(Node ast)
  {
    NdStremePrimitives.loadPrimitives(streme.globalEnv(), streme);
    NdQueryPrimitives.loadAstPrimitives(streme.globalEnv(), compiler);
    streme.readEvalPrint("(load \"lang/core.str\")");
    streme.readEvalPrint("(load \"lang/java.str\")");
    streme.readEvalPrint("(load \"lang/query.str\")");
    streme.readEvalPrint("(load \"lang/transform.str\")");
    streme.globalEnv().add(new Sym("*ast*"), ast);
    streme.readEvalPrint("(define *analysis* (analyze-ast *ast*))");
  }

  public <T> List<T> query(String query, Class<T> c)
  {
    final Object dataQuery = streme.read(new StringReader(query));
    return dataQuery(dataQuery, c);
  }

  public <T> List<T> dataQuery(final Object dataQuery, Class<T> c)
  {
    final TCont f = new TCont()
    {
      public Callable<Callable> call(Object value)
      {
        if (value != null)
        {
          throw new StremeException("error executing query " + dataQuery + ": " + value);
        }
        return null;
      }
    };
    final List<T> result = new ArrayList<T>();
    TCont.trampoline(streme.evaluateData(dataQuery, streme.globalEnv(), new TSuccess()
    {
      public Callable<Callable> call(Object value, TCont fail)
      {
        result.add((T) value);
        return fail.call(null);
      }}, f));
    return result;
  }

  public Object query(String query)
  {
    return query(query, Object.class);
  }

  public List<Node> queryNodes(String query)
  {
    return query(query, Node.class);
  }

  public Node queryNode(String query)
  {
    List<Node> nodes = queryNodes(query);
    if (nodes.isEmpty())
    {
      return null;
    }
    if (nodes.size() > 1)
    {
      throw new StremeException("expected unique node, got " + nodes);
    }
    return nodes.get(0);
  }

  public void shutdown()
  {
    streme.shutdown();
  }

  public static void main(String[] args)
  {
    String source = "(define fib (lambda (n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))) (fib 30)";
    NdAnalysisStreme querier = new NdAnalysisStreme(source);
    System.out.println(querier.queryNodes("(a-ref)"));
  }
}
