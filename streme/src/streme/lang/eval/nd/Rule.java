package streme.lang.eval.nd;

import java.util.Map;
import java.util.concurrent.Callable;

import streme.lang.TCont;
import streme.lang.data.Sym;
import streme.lang.eval.MapEnv;

public abstract class Rule
{
  public abstract Callable<Callable> apply(Object pattern, Map frame, Map<Sym, Sym> r, MapEnv env, TSuccess success, TCont fail);
}
