package streme.lang.eval.beaver;

import java.util.concurrent.Callable;

import streme.lang.StremeException;
import streme.lang.TCont;

public abstract class TSuccess
{
  public static void trampoline(Callable<Callable> c)
  {
    try
    {
      while (c != null)
      {
        c = c.call();
      }
    }
    catch (Exception e)
    {
      throw new StremeException("trampoline", e);
    }
  }

  public abstract Callable<Callable> call(Object value, TCont fail);
}