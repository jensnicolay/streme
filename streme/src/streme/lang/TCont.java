package streme.lang;

import java.util.concurrent.Callable;

public abstract class TCont
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

  public abstract Callable<Callable> call(Object value);
}
