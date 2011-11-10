package streme.lang.eval.tanfe;

import java.io.Reader;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;

import streme.lang.TCont;
import streme.lang.eval.DataEvaluator;
import streme.lang.eval.LstEnv;


public interface StremeContext extends DataEvaluator<LstEnv>
{
  Object read(Reader reader);
  ExecutorService executor();
  Callable<Callable> evaluateData(Object data, LstEnv env, TCont cont);
}
