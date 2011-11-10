package streme.lang.eval.beaver;

import java.util.concurrent.Callable;

import streme.lang.TCont;
import streme.lang.eval.LstEnv;

public interface Exp
{
  Callable<Callable> eval(LstEnv env, TCont cont);
}
