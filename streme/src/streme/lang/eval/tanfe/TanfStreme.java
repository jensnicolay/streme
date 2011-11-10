package streme.lang.eval.tanfe;

import java.io.InputStreamReader;
import java.io.Reader;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;

import jsr166y.ForkJoinPool;
import streme.lang.TCont;
import streme.lang.ast.Literal;
import streme.lang.ast.Literal.Kind;
import streme.lang.ast.Node;
import streme.lang.ast.analysis.AnfConverter;
import streme.lang.ast.analysis.RenamingStrategy;
import streme.lang.ast.impl.AlphaConverter;
import streme.lang.ast.impl.StremeDataCompiler;
import streme.lang.data.Data;
import streme.lang.data.Parser2;
import streme.lang.data.Sym;
import streme.lang.eval.LstEnv;
import streme.lang.eval.MacroExpander;

public class TanfStreme implements StremeContext
{
  private final Parser2 parser = new Parser2();
  private final StremeDataCompiler compiler = new StremeDataCompiler();
  private final Reader reader = new InputStreamReader(System.in);
  private final AstEvaluator evaluator;
  private final ExecutorService executor;
  private final MacroExpander expander = new MacroExpander(this);
  private final AlphaConverter alphaConverter = new AlphaConverter(RenamingStrategy.NUMBER_RENAMING_STRATEGY);
  private final AnfConverter anfConverter = new AnfConverter(RenamingStrategy.NUMBER_RENAMING_STRATEGY);
  private final LstEnv globalEnv = new LstEnv();

  private class RepCont extends TCont
  {
    public Callable<Callable> call(Object value)
    {
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
            return TanfStreme.this.evaluateData(data, globalEnv, RepCont.this);
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
  
  public TanfStreme()
  {
    this(new ForkJoinPool());
  }

  public TanfStreme(ExecutorService executor)
  {
    super();
    this.executor = executor;
    this.evaluator = new TanfEvaluator(executor);
    StremePrimitives.loadPrimitives(globalEnv, this);
  }

  public void startRepl()
  {
    Node value = new Literal(new Sym("TanfStreme\n" + executor), Kind.CONSTANT);
    while (true)
    {
      try
      {
        TCont.trampoline(evaluator.eval(value, globalEnv, new RepCont()));
      }
      catch (Exception e)
      {
        e.printStackTrace(System.out);
        value = Literal.UNSPECIFIED;
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
    Object expanded = expander.rewrite(data);
    Node ast = compiler.compile(expanded);
    Node alphaAst = alphaConverter.rewrite(ast);
    Node anf = anfConverter.rewrite(alphaAst);
    return evaluator.eval(anf, env, cont);
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
    TanfStreme tanfStreme = new TanfStreme(executor);
    tanfStreme.readEvalPrint("(load \"lang/core.str\")");
    tanfStreme.readEvalPrint("(load \"lang/java.str\")");
    tanfStreme.startRepl();
  }
}
