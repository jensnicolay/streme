package streme.lang.eval.nd;

import java.io.Reader;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;

import streme.lang.TCont;
import streme.lang.ast.Node;
import streme.lang.eval.DataEvaluator;
import streme.lang.eval.MapEnv;


public interface NdStremeContext extends DataEvaluator<MapEnv>
{
  Object read(Reader reader);
  Node compile(Object data);
  ExecutorService executor();
  Callable<Callable> evaluateData(Object data, MapEnv env, TSuccess cont, TCont fail);
}
