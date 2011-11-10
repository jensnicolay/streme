package streme.lang.eval.nd;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.Callable;

import streme.lang.StremeException;
import streme.lang.TCont;
import streme.lang.ast.Application;
import streme.lang.ast.AstAnalyzer;
import streme.lang.ast.Begin;
import streme.lang.ast.Define;
import streme.lang.ast.If;
import streme.lang.ast.Lambda;
import streme.lang.ast.Literal;
import streme.lang.ast.Node;
import streme.lang.ast.Ref;
import streme.lang.ast.SetVar;
import streme.lang.ast.Var;
import streme.lang.data.Sym;
import streme.lang.eval.MapEnv;

public class NdExpAnalyzer implements AstAnalyzer<Exp>
{
  public Exp analyze(Node node)
  {
    switch (node.type())
    {
      case LITERAL:
      {
        final Literal literal = (Literal) node;
        return new Exp()
        {
          public Callable<Callable> eval(MapEnv env, TSuccess success, TCont fail)
          {
            return success.call(literal.getValue(), fail);
          }
        };
      }
      case REF:
      {
        final Ref ref = (Ref) node;
        final Sym name = ref.getName();
        String str = name.getName();
        if (str.startsWith("*") && str.endsWith("*"))
        {
          return new Exp()
          {
            public Callable<Callable> eval(MapEnv env, TSuccess success, TCont fail)
            {
              MapEnv dynamic = (MapEnv) env.get(new Sym("dynamic-env"));
              if (dynamic == null)
              {
                dynamic = env;
              }
              do
              {
                Object value = dynamic.get(name);
                if (value != null)
                {
                  return success.call(value, fail);
                }
                dynamic = (MapEnv) dynamic.get(new Sym("dynamic-env"));
              }
              while (dynamic != null);
              return fail.call("unbound dynamic variable " + name);
            }
          };
        }
        return new Exp()
        {
          public Callable<Callable> eval(MapEnv env, TSuccess success, TCont fail)
          {
            Object value = env.get(name);
            if (value == null)
            {
              return fail.call("unbound variable " + name);
            }
            return success.call(value, fail);
          }
        };
      }
      case LAMBDA:
      {
        final Lambda lambda = (Lambda) node;
        return new Exp()
        {
          public Callable<Callable> eval(MapEnv env, TSuccess success, TCont fail)
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
              return success.call(new ClosureN(names, analyze(lambda.getBody()), env), fail);
            }
            else
            {
              return success.call(new ClosureV(names, varparam.getName(), analyze(lambda.getBody()), env), fail);
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
          public Callable<Callable> eval(final MapEnv env, final TSuccess success, TCont fail)
          {
            return cond.eval(env, new TSuccess()
            {
              public Callable<Callable> call(Object value, TCont fail2)
              {
                return (Boolean.FALSE.equals(value) ? alternate : consequent).eval(env, success, fail2);
              }
            }, fail);
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
          public Callable<Callable> eval(final MapEnv env, final TSuccess success, TCont fail)
          {
            class BeginCont extends TSuccess
            {
              final int i;

              BeginCont(int i)
              {
                super();
                this.i = i;
              }

              public Callable<Callable> call(Object value, TCont fail2)
              {
                return exps2[i].eval(env, i + 1 == length ? success : new BeginCont(i + 1), fail2);
              }
            }
            return exps2[0].eval(env, new BeginCont(1), fail);
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
          public Callable<Callable> eval(final MapEnv env, final TSuccess success, TCont fail)
          {
            return exp.eval(env, new TSuccess()
            {
              public Callable<Callable> call(Object value, TCont fail2)
              {
                env.add(name, value);
                return success.call(Void.TYPE, fail2);
              }
            }, fail);
          };
        };
      }
      case APPLICATION:
      {
        Application application = (Application) node;
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
          public Callable<Callable> eval(final MapEnv env, final TSuccess success, TCont fail)
          {
            return operator.eval(env, new TSuccess()
            {
              public Callable<Callable> call(final Object operator2, final TCont fail2)
              {
                if (length == 0)
                {
                  return new Callable<Callable>()
                  {
                    public Callable call() throws Exception
                    {
                      return ((Procedure) operator2).apply0(env, success, fail2);
                    }
                  };
                }
                class OperandsCont extends TSuccess
                {
                  final int i;
                  final List<Object> operands3;

                  OperandsCont(int i, List<Object> operands3)
                  {
                    super();
                    this.i = i;
                    this.operands3 = operands3;
                  }

                  public Callable<Callable> call(final Object value, final TCont fail3)
                  {
                    if (i + 1 == length)
                    {
                      return new Callable<Callable>()
                      {
                        public Callable call() throws Exception
                        {
                          switch (length)
                          {
                            case 1:
                              return ((Procedure) operator2).apply1(value, env, success, fail3);
                            case 2:
                              return ((Procedure) operator2).apply2(operands3.get(0), value, env, success, fail3);
                            case 3:
                              return ((Procedure) operator2).apply3(operands3.get(0), operands3.get(1), value, env,
                                  success, fail3);
                            default:
                            {
                              List<Object> operands4 = new ArrayList<Object>(operands3);
                              operands4.add(value);
                              return ((Procedure) operator2).applyN(operands4.toArray(new Object[length]), env,
                                  success, fail3);
                            }
                          }
                        }
                      };
                    }
                    List<Object> operands4 = new ArrayList<Object>(operands3);
                    operands4.add(value);
                    return operands2[i + 1].eval(env, new OperandsCont(i + 1, operands4), fail3);
                  }
                }
                return operands2[0].eval(env, new OperandsCont(0, Collections.emptyList()), fail2);
              }
            }, fail);
          }
        };
      }
      case SETVAR:
      {
        SetVar setVar = (SetVar) node;
        final Exp exp = analyze(setVar.getValue());
        final Sym name = setVar.getVar().getName();
        return new Exp()
        {
          public Callable<Callable> eval(final MapEnv env, final TSuccess success, TCont fail)
          {
            return exp.eval(env, new TSuccess()
            {
              public Callable<Callable> call(Object value, final TCont fail2)
              {
                final Object oldValue = env.get(name);
                System.out.println("now putting " + value + " remembering " + oldValue);
                env.add(name, value);
                return success.call(Void.TYPE, new TCont()
                {
                  public Callable<Callable> call(Object value)
                  {
                    System.out.println("putting back " + oldValue + " overwriting " + env.get(name));
                    env.add(name, oldValue);
                    return fail2.call(null);
                  }
                });
              }
            }, fail);
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
