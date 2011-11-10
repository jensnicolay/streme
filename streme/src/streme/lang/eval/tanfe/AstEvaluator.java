package streme.lang.eval.tanfe;

import java.util.concurrent.Callable;

import streme.lang.TCont;
import streme.lang.ast.Node;
import streme.lang.eval.LstEnv;


public interface AstEvaluator
{
  Callable<Callable> eval(Node node, LstEnv env, TCont cont);
}
