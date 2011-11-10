package streme.lang.eval.nd;

import java.util.concurrent.Callable;

import streme.lang.TCont;
import streme.lang.data.Sym;
import streme.lang.eval.MapEnv;

public class ClosureN extends Procedure
{
  private static int c;
  private MapEnv staticEnv;
  private Sym[] params;
  private Exp body;

  public ClosureN(Sym[] params, Exp body, MapEnv env)
  {
    super();
    this.params = params;
    this.body = body;
    this.staticEnv = env;
  }
  
  public Callable<Callable> apply0(MapEnv env, final TSuccess cont, final TCont fail)
  {
    final MapEnv extended = new MapEnv(staticEnv);
    extended.add(new Sym("dynamic-env"), env);
    return body.eval(extended, cont, fail);
  }

  public Callable<Callable> apply1(final Object operand, MapEnv env, final TSuccess cont, final TCont fail)
  {
    final MapEnv extended = new MapEnv(staticEnv);
    extended.add(new Sym("dynamic-env"), env);
    extended.add(params[0], operand);
    return body.eval(extended, cont, fail);
  }

  public Callable<Callable> apply2(final Object operand1, final Object operand2, MapEnv env, final TSuccess cont, final TCont fail)
  {
    final MapEnv extended = new MapEnv(staticEnv);
    extended.add(new Sym("dynamic-env"), env);
    extended.add(params[0], operand1);
    extended.add(params[1], operand2);
    return body.eval(extended, cont, fail);
  }

  public Callable<Callable> apply3(final Object operand1, final Object operand2, final Object operand3, MapEnv env,
      final TSuccess cont, final TCont fail)
  {
    final MapEnv extended = new MapEnv(staticEnv);
    extended.add(new Sym("dynamic-env"), env);
    extended.add(params[0], operand1);
    extended.add(params[1], operand2);
    extended.add(params[2], operand3);
    return body.eval(extended, cont, fail);
  }

  public Callable<Callable> applyN(final Object[] operands, MapEnv env, final TSuccess cont, final TCont fail)
  {
    final MapEnv extended = new MapEnv(staticEnv);
    extended.add(new Sym("dynamic-env"), env);
    for (int i = 0; i < params.length; i++)
    {
      extended.add(params[i], operands[i]);
    }
    return body.eval(extended, cont, fail);
  }
}
