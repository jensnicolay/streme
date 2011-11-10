package streme.lang.analysis;

import java.util.Set;

import streme.lang.StremeException;

public abstract class Procedure
{
  private Object name;
  
  public Procedure(String name)
  {
    super();
    this.name = name;
  }

  public void setName(Object name)
  {
    this.name = name;
  }

  public final String toString()
  {
    return "<procedure " + (name == null ? getClass().getSimpleName() : name) + ">";
  }

  public void apply(Set<Object>[] operands, Time time, BindingEnv bindingEnv, StackCont cont)
  {
    throw new StremeException("not implemented: " + this);
  }
}
