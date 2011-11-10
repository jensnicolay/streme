package streme.lang.eval.nd;

import java.util.concurrent.Callable;

import streme.lang.TCont;
import streme.lang.eval.MapEnv;

public interface Exp
{
  Callable<Callable> eval(MapEnv env, TSuccess success, TCont fail);
}
