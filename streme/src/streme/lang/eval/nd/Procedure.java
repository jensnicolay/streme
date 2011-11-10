package streme.lang.eval.nd;

import java.util.concurrent.Callable;

import streme.lang.StremeException;
import streme.lang.TCont;
import streme.lang.eval.MapEnv;

public abstract class Procedure
{
  private Object name;

  public void setName(Object name)
  {
    this.name = name;
  }

  public final String toString()
  {
    return "<procedure " + (name == null ? getClass().getSimpleName() : name) + ">";
  }

  public Callable<Callable> apply0(MapEnv env, TSuccess cont, TCont fail)
  {
    throw new StremeException("apply: illegal number of arguments (0) for " + this);
  }

  public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
  {
    throw new StremeException("apply: illegal number of arguments (1) for " + this);
  }

  public Callable<Callable> apply2(Object operand1, Object operand2, MapEnv env, TSuccess cont, TCont fail)
  {
    throw new StremeException("apply: illegal number of arguments (2) for " + this);
  }

  public Callable<Callable> apply3(Object operand1, Object operand2, Object operand3, MapEnv env, TSuccess cont, TCont fail)
  {
    throw new StremeException("apply: illegal number of arguments (3) for " + this);
  }

  public Callable<Callable> applyN(Object[] operands, MapEnv env, TSuccess cont, TCont fail)
  {
    throw new StremeException("apply: illegal number of arguments (" + operands.length + ") for " + this);
  }
  
}
