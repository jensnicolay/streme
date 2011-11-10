package streme.lang.eval.tanfe;

import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.atomic.AtomicInteger;

import streme.lang.StremeException;
import streme.lang.TCont;
import streme.lang.ast.Application;
import streme.lang.ast.Define;
import streme.lang.ast.Future;
import streme.lang.ast.If;
import streme.lang.ast.Lambda;
import streme.lang.ast.Let;
import streme.lang.ast.Literal;
import streme.lang.ast.Node;
import streme.lang.ast.Ref;
import streme.lang.ast.SetVar;
import streme.lang.ast.Var;
import streme.lang.data.Pair;
import streme.lang.data.Sym;
import streme.lang.eval.LstEnv;

public class TanfEvaluator implements AstEvaluator
{
  private final int maxFutures;
  private final AtomicInteger futures = new AtomicInteger();
  private final ExecutorService executor;

  public TanfEvaluator(ExecutorService executor)
  {
    this(executor, Runtime.getRuntime().availableProcessors() * 2);
  }

  public TanfEvaluator(ExecutorService executor, int maxFutures)
  {
    super();
    this.executor = executor;
    this.maxFutures = maxFutures;
  }

  private Object atomEval(Node node, LstEnv env)
  {
    switch (node.type())
    {
      case LITERAL:
        return ((Literal) node).getValue();
      case REF:
      {
        Ref ref = (Ref) node;
        Sym name = ref.getName();
        Object lookup = env.get(name);
        if (Boolean.FALSE.equals(lookup))
        {
          throw new StremeException("unbound variable '" + name + "'");
        }
        return ((Pair) lookup).cdr();
      }
      case LAMBDA:
      {
        Lambda lambda = (Lambda) node;
        Var[] params = lambda.getParams();
        Sym[] names = new Sym[params.length];
        for (int i = 0; i < params.length; i++)
        {
          names[i] = params[i].getName();
        }
        Var varparam = lambda.getVarparam();
        if (varparam == null)
        {
          return new ClosureN(names, lambda.getBody(), env, this);
        }
        else
        {
          return new ClosureV(names, varparam.getName(), lambda.getBody(), env, this);
        }
      }
      case SETVAR:
      {
        SetVar setVar = (SetVar) node;
        Sym name = setVar.getVar().getName();
        Object value = atomEval(setVar.getValue(), env);
        Object lookup = env.get(name);
        if (Boolean.FALSE.equals(lookup))
        {
          throw new StremeException("unbound variable '" + name + "'");
        }
        ((Pair) lookup).setCdr(value);
        return Void.TYPE;
      }
      default:
        throw new StremeException("cannot atomically evaluate " + node);
    }
  }

  public Callable<Callable> eval(Node node, final LstEnv env, final TCont cont)
  {
    switch (node.type())
    {
      case LITERAL:
      case REF:
      case LAMBDA:
      case SETVAR:
      {
        return cont.call(atomEval(node, env));
      }
      case APPLICATION:
      {
        Application application = (Application) node;
        return apply(application, env, cont);
      }
      case DEFINE:
      {
        Define define = (Define) node;
        LstEnv global = env;
        LstEnv parent = global.getParent();
        while (parent != null)
        {
          global = parent;
          parent = parent.getParent();
        }
        Sym name = define.getVar().getName();
        Object evalue = atomEval(define.getValue(), env);
        global.add(name, evalue);
        if (evalue instanceof ClosureN)
        {
          ((ClosureN) evalue).setName(name);
        }
        return cont.call(Void.TYPE);
      }
      case LET:
      {
        Let let = (Let) node;
        final Sym name = let.getName(0).getName();
        Node value = let.getValue(0);
        final Node body = let.getBody();
        switch (let.getKind())
        {
          case LET:
          {
            return eval(value, env, new TCont()
            {
              public Callable<Callable> call(final Object value)
              {
                return new Callable<Callable>()
                {
                  public Callable call() throws Exception
                  {
                    LstEnv extended = new LstEnv(env);
                    extended.add(name, value);
                    return eval(body, extended, cont);
                  }
                };
              }
            });
          }
          case LETREC:
          {
            LstEnv extended = new LstEnv(env);
            Pair binding = extended.add(name, null);
            Object evalue = atomEval(value, extended);
            binding.setCdr(evalue);
            return eval(body, extended, cont);
          }
          default:
            throw new StremeException("cannot handle let kind " + let);
        }
      }
      case IF:
      {
        If ff = (If) node;
        Object value = atomEval(ff.getCondition(), env);
        if (Boolean.FALSE.equals(value))
        {
          return eval(ff.getAlternate(), env, cont);
        }
        else
        {
          return eval(ff.getConsequent(), env, cont);
        }
      }
      case FUTURE:
      {
        Future future = (Future) node;
        final Node value = future.getValue();
        if (futures.get() >= maxFutures)
        // if (true)
        {
          return eval(value, env, cont);
        }
        else
        {
          int f = futures.incrementAndGet();
          // System.out.println("future " + f);
          return cont.call(executor.submit(new Callable<Object>()
          {
            public Object call()
            {
              final Object[] result = new Object[1];
              TCont.trampoline(eval(value, env, new TCont()
              {
                public Callable<Callable> call(Object value)
                {
                  futures.decrementAndGet();
                  result[0] = value;
                  return null;
                }
              }));
              return result[0];
            }
          }));
        }
      }
      default:
      {
        throw new StremeException("cannot handle " + node);
      }
    }
  }

  private Callable<Callable> apply(final Application application, final LstEnv env, final TCont cont)
  {
    //System.out.println(Thread.currentThread() + " apply " + application + " " + cont);
    return new Callable<Callable>()
    {
      public Callable call() throws Exception
      {
        Procedure eoperator = (Procedure) atomEval(application.getOperator(), env);
        Node[] operands = application.getOperands();
        int numOperands = operands.length;
        switch (numOperands)
        {
          case 0:
          {
            return eoperator.apply0(env, cont);
          }
          case 1:
          {
            Object eoperand = atomEval(operands[0], env);
            return eoperator.apply1(eoperand, env, cont);
          }
          case 2:
          {
            Object eoperand1 = atomEval(operands[0], env);
            Object eoperand2 = atomEval(operands[1], env);
            return eoperator.apply2(eoperand1, eoperand2, env, cont);
          }
          case 3:
          {
            Object eoperand1 = atomEval(operands[0], env);
            Object eoperand2 = atomEval(operands[1], env);
            Object eoperand3 = atomEval(operands[2], env);
            return eoperator.apply3(eoperand1, eoperand2, eoperand3, env, cont);
          }
          default:
          {
            Object[] eoperands = new Object[numOperands];
            for (int i = 0; i < numOperands; i++)
            {
              eoperands[i] = atomEval(operands[i], env);
            }
            return eoperator.applyN(eoperands, env, cont);
          }
        }
      }
    };
  }

  public static void main(String[] args)
  {
  }
}
