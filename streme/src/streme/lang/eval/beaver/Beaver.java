package streme.lang.eval.beaver;

import java.io.InputStreamReader;
import java.io.Reader;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;

import jsr166y.ForkJoinPool;
import streme.lang.StremeException;
import streme.lang.TCont;
import streme.lang.ast.Literal;
import streme.lang.ast.Node;
import streme.lang.ast.impl.StremeDataCompiler;
import streme.lang.data.Data;
import streme.lang.data.Sym;
import streme.lang.eval.LstEnv;
import streme.lang.eval.MacroExpander;
import streme.lang.eval.tanfe.StremeContext;

public class Beaver implements StremeContext
{
  private final BeaverParser parser = new BeaverParser();
  private final StremeDataCompiler compiler = new StremeDataCompiler();
//  private final BeaverExpAnalyzer expAnalyzer = new BeaverExpAnalyzer();
  private final Reader reader = new InputStreamReader(System.in);
  private final ExecutorService executor;
  private final MacroExpander expander = new MacroExpander(this);
  private final LstEnv globalEnv = new LstEnv();

  private class RepCont extends TCont
  {
    public Callable<Callable> call(Object value)
    {
      System.out.println(executor);
      if (value != Void.TYPE)
      {
        System.out.println(Data.toString(value));
      }
      else
      {
        System.out.println();
      }
      System.out.print("> ");
      return new Callable<Callable>()
      {
        public Callable call() throws Exception
        {
          try
          {
            Object data = read(reader);
            return Beaver.this.evaluateData(data, globalEnv, RepCont.this);
          }
          catch (Exception e)
          {
            e.printStackTrace(System.out);
            return RepCont.this.call(Void.TYPE);
          }
        }
      };
    }
  }

  public Beaver()
  {
    this(new ForkJoinPool());
  }
  
  public void shutdown()
  {
    executor.shutdown();
  }

  public Beaver(ExecutorService executor)
  {
    super();
    this.executor = executor;
    expander.setLetrecToLambda(false);
    BeaverPrimitives.loadPrimitives(globalEnv, this);
  }

  public void startRepl()
  {
    Sym value = new Sym("proto-Beaver, memory " + Runtime.getRuntime().maxMemory() / 1024 / 1024 + "M");
    while (true)
    {
      try
      {
        TCont.trampoline(new RepCont().call(value));
      }
      catch (Exception e)
      {
        e.printStackTrace(System.out);
      }
    }
  }

  public LstEnv globalEnv()
  {
    return globalEnv;
  }

  public Object read(Reader reader)
  {
    return parser.parse(reader);
  }

  public Callable<Callable> evaluateData(Object data, LstEnv env, TCont cont)
  {
    Node ast = compile(data);
    Exp exp = new BeaverExpAnalyzer(executor).analyze(ast);
    return exp.eval(env, cont);
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

  public Object evaluateData(Object data, LstEnv env)
  {
    final Object[] result = new Object[1];
    TCont.trampoline(evaluateData(data, env, new TCont()
    {
      public Callable<Callable> call(Object value)
      {
        result[0] = value;
        return null;
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
    Beaver beaver = new Beaver(executor);
    beaver.readEvalPrint("(load \"lang/core.str\")");
    beaver.readEvalPrint("(load \"lang/java.str\")");
    beaver.startRepl();
  }
}
