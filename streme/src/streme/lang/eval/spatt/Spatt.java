package streme.lang.eval.spatt;

import java.io.InputStreamReader;
import java.io.Reader;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;

import jsr166y.ForkJoinPool;
import streme.lang.TCont;
import streme.lang.ast.Node;
import streme.lang.ast.impl.StremeDataCompiler;
import streme.lang.ast.impl.StremeSpDataCompiler2;
import streme.lang.data.Data;
import streme.lang.data.DataTemplate;
import streme.lang.data.Lst;
import streme.lang.data.Parser2;
import streme.lang.data.SpParser2;
import streme.lang.data.Sym;
import streme.lang.eval.LstEnv;
import streme.lang.eval.MacroExpander;
import streme.lang.eval.tanfe.StremeContext;

public class Spatt implements StremeContext
{
  private final Parser2 parser = new Parser2();
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
      //System.out.println(executor);
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
            return Spatt.this.evaluateData(data, globalEnv, RepCont.this);
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

  public Spatt()
  {
    this(new ForkJoinPool());
  }
  
  public void shutdown()
  {
    executor.shutdown();
  }

  public Spatt(ExecutorService executor)
  {
    super();
    this.executor = executor;
    expander.setLetToLambda(true);
    expander.setLetrecToLambda(true);
    SpattPrimitives.loadPrimitives(globalEnv, this);
    readEvalPrint("(load \"lang/core.str\")");
    readEvalPrint("(load \"lang/srfi1.str\")");
    readEvalPrint("(load \"lang/java.str\")");
    readEvalPrint("(load \"lang/ast.str\")");
    readEvalPrint("(load \"lang/transform.str\")");
    readEvalPrint("(load \"lang/ipda.str\")");
  }

  public void startRepl()
  {
    Sym value = new Sym("SPATT, memory " + Runtime.getRuntime().maxMemory() / 1024 / 1024 + "M");
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
    return evaluateAst(ast, env, cont);
  }

  public Callable<Callable> evaluateAst(Node ast, LstEnv env, TCont cont)
  {
    Exp exp = new SpattExpAnalyzer(executor).analyze(ast);
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
    Object result = readEval(in);
    return Data.toString(result);
  }

  public Object readEval(CharSequence in, Object... templateArgs)
  {
    // in the future (hopefully): macroexpansion at AST level
    // sp bogus here after data macroexpansion
    SpParser2 parser = new SpParser2(in);
    List<Object> data = parser.all();
    DataTemplate template = new DataTemplate(Lst.valueOf(data));
    Lst substituted = (Lst) template.substitute(templateArgs);
    Lst expanded = (Lst) expander.rewrite(substituted);
    StremeSpDataCompiler2 compiler = new StremeSpDataCompiler2(parser.getSps());
    Node ast = compiler.compileBody(expanded);
    Object result = evaluateAst(ast, globalEnv);
    return result;
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

  public Object evaluateAst(Node ast, LstEnv env)
  {
    final Object[] result = new Object[1];
    TCont.trampoline(evaluateAst(ast, env, new TCont()
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
    Spatt spatt = new Spatt(executor);
    spatt.startRepl();
  }
}
