package streme.lang.eval.nd;

import java.io.InputStreamReader;
import java.io.Reader;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;

import jsr166y.ForkJoinPool;
import streme.lang.StremeException;
import streme.lang.TCont;
import streme.lang.ast.Literal;
import streme.lang.ast.Literal.Kind;
import streme.lang.ast.Node;
import streme.lang.ast.impl.StremeDataCompiler;
import streme.lang.data.Data;
import streme.lang.data.Parser2;
import streme.lang.data.Sym;
import streme.lang.eval.MacroExpander;
import streme.lang.eval.MapEnv;

public class NdStreme implements NdStremeContext
{
  private final Parser2 parser = new Parser2();
  private final StremeDataCompiler compiler = new StremeDataCompiler();
  private final NdExpAnalyzer expAnalyzer = new NdExpAnalyzer();
  private final Reader reader = new InputStreamReader(System.in);
  private final ExecutorService executor;
  private final MacroExpander expander = new MacroExpander(this);
  private final MapEnv globalEnv = new MapEnv();

  private class RepCont extends TSuccess
  {
    public Callable<Callable> call(Object value, final TCont fail)
    {
      if (value != Void.TYPE)
      {
        System.out.println(Data.toString(value));
      }
      else
      {
        System.out.println();
      }
      final TCont repFail;
      if (fail != null)
      {
        return fail.call(null);
      }
      else
      {
        repFail = new TCont()
        {
          public Callable<Callable> call(Object value)
          {
            if (value != null)
            {
              System.out.println(value);
            }
            return expAnalyzer.analyze(Literal.UNSPECIFIED).eval(globalEnv, new RepCont(), null);
          }
        };
      }
      System.out.print("> ");
      return new Callable<Callable>()
      {
        public Callable call() throws Exception
        {
          try
          {
            Object data = read(reader);
            return NdStreme.this.evaluateData(data, globalEnv, RepCont.this, repFail);
          }
          catch (Exception e)
          {
            e.printStackTrace(System.out);
            return RepCont.this.call(Void.TYPE, null);
          }
        }
      };
    }
  }

  public NdStreme()
  {
    this(new ForkJoinPool());
  }
  
  public void shutdown()
  {
    executor.shutdown();
  }

  public NdStreme(ExecutorService executor)
  {
    super();
    this.executor = executor;
  }

  public void startRepl()
  {
    Node value = new Literal(new Sym("NdStreme\n" + executor), Kind.CONSTANT);
    while (true)
    {
      try
      {
        Exp exp = expAnalyzer.analyze(value);
        TCont.trampoline(exp.eval(globalEnv, new RepCont(), null));
      }
      catch (Exception e)
      {
        e.printStackTrace(System.out);
        value = Literal.UNSPECIFIED;
      }
    }
  }

  public MapEnv globalEnv()
  {
    return globalEnv;
  }

  public Object read(Reader reader)
  {
    return parser.parse(reader);
  }

  public Callable<Callable> evaluateData(Object data, MapEnv env, TSuccess cont, TCont fail)
  {
    Node ast = compile(data);
    Exp exp = expAnalyzer.analyze(ast);
    return exp.eval(env, cont, fail);
  }

  public Node compile(Object data)
  {
    Object expanded = expander.rewrite(data);
    Node ast = compiler.compile(expanded);
    return ast;
  }

  public String readEvalPrint(CharSequence in)
  {
    Object data = parser.parse(in);
    Object result = evaluateData(data, globalEnv);
    return Data.toString(result);
  }

  public Object evaluateData(Object data, MapEnv env)
  {
    final Object[] result = new Object[1];
    TCont.trampoline(evaluateData(data, env, new TSuccess()
    {
      public Callable<Callable> call(Object value, TCont fail)
      {
        result[0] = value;
        return null;
      }
    }, new TCont()
    {
      public Callable<Callable> call(Object value)
      {
        throw new StremeException(String.valueOf(value));
      }
    }));
    return result[0];
  }

  public ExecutorService executor()
  {
    return executor;
  }

  public static void main(String[] args)
  {
    ForkJoinPool executor = new ForkJoinPool();
    NdStreme ndStreme = new NdStreme(executor);
    MapEnv env = ndStreme.globalEnv();
    NdStremePrimitives.loadPrimitives(env, ndStreme);
    NdQueryPrimitives.loadSQueryPrimitives(env);
    NdQueryPrimitives.loadAstPrimitives(env, new StremeDataCompiler());
    ndStreme.readEvalPrint("(load \"lang/core.str\")");
    ndStreme.readEvalPrint("(load \"lang/java.str\")");
    ndStreme.readEvalPrint("(load \"lang/query.str\")");
    ndStreme.readEvalPrint("(load \"lang/transform.str\")");
    ndStreme.startRepl();
  }
}
