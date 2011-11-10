package streme.lang.eval.tanfe;

import java.util.concurrent.Callable;

import streme.lang.TCont;
import streme.lang.ast.Node;
import streme.lang.eval.LstEnv;

public class AstEvaluators
{
  public static Object evaluate(AstEvaluator evaluator, Node ast, LstEnv env)
  {
    final Object[] result = new Object[1];
    TCont.trampoline(evaluator.eval(ast, env, new TCont()
    {
      public Callable<Callable> call(Object value)
      {
        result[0] = value;
        return null;
      }
    }));
    return result[0];
  }

  private AstEvaluators()
  {
    super();
  }
}
