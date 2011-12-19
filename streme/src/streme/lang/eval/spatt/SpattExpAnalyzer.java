package streme.lang.eval.spatt;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.atomic.AtomicInteger;

import streme.lang.StremeException;
import streme.lang.TCont;
import streme.lang.ast.Application;
import streme.lang.ast.AstAnalyzer;
import streme.lang.ast.Begin;
import streme.lang.ast.Binding;
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
import streme.lang.eval.MapEnv;
import streme.lang.eval.nd.TSuccess;

public class SpattExpAnalyzer implements AstAnalyzer<Exp>
{
  private final AtomicInteger futures = new AtomicInteger();
  private final int maxFutures = Runtime.getRuntime().availableProcessors() * 2;
  private ExecutorService executor;

  public SpattExpAnalyzer(ExecutorService executor)
  {
    super();
    this.executor = executor;
  }

  public Exp analyze(Node node)
  {
    switch (node.type())
    {
      case LITERAL:
      {
        final Literal literal = (Literal) node;
        return new Exp()
        {
          public Callable<Callable> eval(LstEnv env, TCont cont)
          {
            return cont.call(literal.getValue());
          }
        };
      }
      case REF:
      {
        final Ref ref = (Ref) node;
        final Sym sym = ref.getName();
        return new Exp()
        {
          public Callable<Callable> eval(LstEnv env, TCont cont)
          {
            Object value = env.get(sym);
            if (Boolean.FALSE.equals(value))
            {
              throw new StremeException("unbound variable " + sym);
            }
            return cont.call(((Pair) value).cdr());
          }
        };
      }
      case LAMBDA:
      {
        final Lambda lambda = (Lambda) node;
        return new Exp()
        {
          public Callable<Callable> eval(LstEnv env, TCont cont)
          {
            Var[] params = lambda.getParams();
            Sym[] names = new Sym[params.length];
            for (int i = 0; i < params.length; i++)
            {
              names[i] = params[i].getName();
            }
            Var varparam = lambda.getVarparam();
            if (varparam == null)
            {
              return cont.call(new ClosureN(names, analyze(lambda.getBody()), env));
            }
            else
            {
              return cont.call(new ClosureV(names, varparam.getName(), analyze(lambda.getBody()), env));
            }
          }
        };
      }
      case IF:
      {
        If ff = (If) node;
        final Exp cond = analyze(ff.getCondition());
        final Exp consequent = analyze(ff.getConsequent());
        final Exp alternate = analyze(ff.getAlternate());
        return new Exp()
        {
          public Callable<Callable> eval(final LstEnv env, final TCont cont)
          {
            return cond.eval(env, new TCont()
            {
              public Callable<Callable> call(Object value)
              {
                return (Boolean.FALSE.equals(value) ? alternate : consequent).eval(env, cont);
              }
            });
          }
        };
      }
      case BEGIN:
      {
        Begin begin = (Begin) node;
        Node[] exps = begin.getExps();
        final int length = exps.length;
        final Exp[] exps2 = new Exp[length];
        for (int i = 0; i < length; i++)
        {
          exps2[i] = analyze(exps[i]);
        }
        if (length == 1)
        {
          return exps2[0];
        }
        return new Exp()
        {
          public Callable<Callable> eval(final LstEnv env, final TCont cont)
          {
            class BeginCont extends TCont
            {
              final int i;

              BeginCont(int i)
              {
                super();
                this.i = i;
              }

              public Callable<Callable> call(Object value)
              {
                return exps2[i].eval(env, i + 1 == length ? cont : new BeginCont(i + 1));
              }
            }
            return exps2[0].eval(env, new BeginCont(1));
          }
        };
      }
      case DEFINE:
      {
        Define define = (Define) node;
        final Exp exp = analyze(define.getValue());
        final Sym name = define.getVar().getName();
        return new Exp()
        {
          public Callable<Callable> eval(final LstEnv env, final TCont cont)
          {
            return exp.eval(env, new TCont()
            {
              public Callable<Callable> call(Object value)
              {
                env.add(name, value);
                if (value instanceof Procedure)
                {
                  ((Procedure) value).setName(name);
                }
                return cont.call(Void.TYPE);
              }
            });
          };
        };
      }
      case SETVAR:
      {
        SetVar setVar = (SetVar) node;
        final Exp exp = analyze(setVar.getValue());
        final Sym name = setVar.getVar().getName();
        return new Exp()
        {
          public Callable<Callable> eval(final LstEnv env, final TCont cont)
          {
            return exp.eval(env, new TCont()
            {
              public Callable<Callable> call(Object value)
              {
                Object lookup = env.get(name);
                if (Boolean.FALSE.equals(lookup))
                {
                  throw new StremeException("unbound variable '" + name + "'");
                }
                ((Pair) lookup).setCdr(value);
                return cont.call(Void.TYPE);
              }
            });
          };
        };
      }
      // case LET:
      // {
      // Let let = (Let) node;
      // switch (let.getKind())
      // {
      // case LETREC:
      // {
      // Binding[] bindings = let.getBindings();
      // final int numBindings = bindings.length;
      // final Sym[] names = new Sym[numBindings];
      // final Exp[] bindingExps = new Exp[numBindings];
      // final Exp body = analyze(let.getBody());
      // for (int i = 0; i < numBindings; i++)
      // {
      // names[i] = bindings[i].getVar().getName();
      // bindingExps[i] = analyze(bindings[i].getValue());
      // }
      // return new Exp()
      // {
      // public Callable<Callable> eval(final LstEnv env, final TCont cont)
      // {
      // final LstEnv extended = new LstEnv(env);
      // final Pair[] extensions = new Pair[numBindings];
      // for (int i = 0; i < numBindings; i++)
      // {
      // extensions[i] = extended.add(names[i], null);
      // }
      // class LetrecCont extends TCont
      // {
      // int i;
      //
      // LetrecCont(int i)
      // {
      // super();
      // this.i = i;
      // }
      //
      // public Callable<Callable> call(Object value)
      // {
      // extensions[i].setCdr(value);
      // if (i + 1 == numBindings)
      // {
      // return body.eval(extended, cont);
      // }
      // else
      // {
      // return bindingExps[i + 1].eval(extended, new LetrecCont(i + 1));
      // }
      // }
      // }
      // return bindingExps[0].eval(extended, new LetrecCont(0));
      // }
      // };
      // }
      // default: throw new StremeException("cannot handle let type " + let);
      // }
      // }
      case APPLICATION:
      {
        final Application application = (Application) node;
        Node[] operands = application.getOperands();
        final int length = operands.length;
        final Exp operator = analyze(application.getOperator());
        final Exp[] operands2 = new Exp[length];
        for (int i = 0; i < length; i++)
        {
          operands2[i] = analyze(operands[i]);
        }
        return new Exp()
        {
          public Callable<Callable> eval(final LstEnv env, final TCont cont)
          {
//            System.out.println(application);
            return operator.eval(env, new TCont()
            {
              public Callable<Callable> call(final Object operator2)
              {
                if (length == 0)
                {
                  return new Callable<Callable>()
                  {
                    public Callable call() throws Exception
                    {
//                       System.out.println("calling " + operator2);
                      return ((Procedure) operator2).apply0(env, cont);
                    }
                  };
                }
                class OperandsCont extends TCont
                {
                  final int i;
                  final List<Object> operands3;

                  OperandsCont(int i, List<Object> operands3)
                  {
                    super();
                    this.i = i;
                    this.operands3 = operands3;
                  }

                  public Callable<Callable> call(final Object value)
                  {
                    if (i + 1 == length)
                    {
                      return new Callable<Callable>()
                      {
                        public Callable call() throws Exception
                        {
//                           System.out.println("calling " + operator2 + " with " + operands3 + " and " + value);
                          switch (length)
                          {
                            case 1:
                              return ((Procedure) operator2).apply1(value, env, cont);
                            case 2:
                              return ((Procedure) operator2).apply2(operands3.get(0), value, env, cont);
                            case 3:
                              return ((Procedure) operator2).apply3(operands3.get(0), operands3.get(1), value, env,
                                  cont);
                            default:
                            {
                              List<Object> operands4 = new ArrayList<Object>(operands3);
                              operands4.add(value);
                              // System.out.println("calling " + operator2 + " with " + operands4);
                              return ((Procedure) operator2).applyN(operands4.toArray(new Object[length]), env, cont);
                            }
                          }
                        }
                      };
                    }
                    List<Object> operands4 = new ArrayList<Object>(operands3);
                    operands4.add(value);
                    return operands2[i + 1].eval(env, new OperandsCont(i + 1, operands4));
                  }
                }
                return operands2[0].eval(env, new OperandsCont(0, Collections.emptyList()));
              }
            });
          }
        };
      }
      case FUTURE:
      {
        Future future = (Future) node;
        final Exp value = analyze(future.getValue());
        return new Exp()
        {
          public Callable<Callable> eval(final LstEnv env, TCont cont)
          {
            if (futures.get() >= maxFutures)
            // if (true)
            {
              return value.eval(env, cont);
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
                  TCont.trampoline(value.eval(env, new TCont()
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
        };
      }
      default:
      {
        throw new StremeException("cannot handle " + node);
      }
    }
  }
}
