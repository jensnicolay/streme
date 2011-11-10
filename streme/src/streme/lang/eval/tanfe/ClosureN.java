package streme.lang.eval.tanfe;

import java.util.concurrent.Callable;

import streme.lang.TCont;
import streme.lang.ast.Node;
import streme.lang.data.Sym;
import streme.lang.eval.LstEnv;

public class ClosureN extends Procedure
{
  private static int c;
  private LstEnv staticEnv;
  private Sym[] params;
  private Node body;
  private TanfEvaluator evaluator;

  public ClosureN(Sym[] params, Node body, LstEnv env, TanfEvaluator evaluator)
  {
    super();
    this.params = params;
    this.body = body;
    this.staticEnv = env;
    this.evaluator = evaluator;
  }

  public Callable<Callable> apply0(LstEnv env, final TCont cont)
  {
    final LstEnv extended = new LstEnv(staticEnv);
    return new Callable<Callable>()
    {
      public Callable call()
      {
        return evaluator.eval(body, extended, cont);
      }
    };
  }

  public Callable<Callable> apply1(final Object operand, LstEnv env, final TCont cont)
  {
    final LstEnv extended = new LstEnv(staticEnv);
    extended.add(params[0], operand);
    return evaluator.eval(body, extended, cont);
  }

  public Callable<Callable> apply2(final Object operand1, final Object operand2, LstEnv env, final TCont cont)
  {
    final LstEnv extended = new LstEnv(staticEnv);
    extended.add(params[0], operand1);
    extended.add(params[1], operand2);
    return evaluator.eval(body, extended, cont);
  }

  public Callable<Callable> apply3(final Object operand1, final Object operand2, final Object operand3, LstEnv env,
      final TCont cont)
  {
    final LstEnv extended = new LstEnv(staticEnv);
    extended.add(params[0], operand1);
    extended.add(params[1], operand2);
    extended.add(params[2], operand3);
    return evaluator.eval(body, extended, cont);
  }

  public Callable<Callable> applyN(final Object[] operands, LstEnv env, final TCont cont)
  {
    final LstEnv extended = new LstEnv(staticEnv);
    for (int i = 0; i < params.length; i++)
    {
      extended.add(params[i], operands[i]);
    }
    return evaluator.eval(body, extended, cont);
  }
}
