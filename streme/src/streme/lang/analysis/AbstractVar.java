package streme.lang.analysis;

import java.util.HashSet;
import java.util.Set;

import streme.lang.ast.Var;

public class AbstractVar<T>
{
  
  public static <T> Set<Var> monovariant(Set<AbstractVar<T>> abstractVars)
  {
    Set<Var> result = new HashSet<Var>();
    for (AbstractVar<T> abstractVar : abstractVars)
    {
      result.add(abstractVar.getVar());
    }
    return result;
  }
  
  private Var var;
  private T context;
  
  public AbstractVar(Var var, T context)
  {
    super();
    this.var = var;
    this.context = context;
  }
  
  public Var getVar()
  {
    return var;
  }
  
  public T getContext()
  {
    return context;
  }
  
  public int hashCode()
  {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((context == null) ? 0 : context.hashCode());
    result = prime * result + ((var == null) ? 0 : var.hashCode());
    return result;
  }

  public boolean equals(Object obj)
  {
    if (this == obj)
      return true;
    if (obj == null)
      return false;
    if (getClass() != obj.getClass())
      return false;
    AbstractVar other = (AbstractVar) obj;
    if (context == null)
    {
      if (other.context != null)
        return false;
    }
    else if (!context.equals(other.context))
      return false;
    if (var == null)
    {
      if (other.var != null)
        return false;
    }
    else if (!var.equals(other.var))
      return false;
    return true;
  }

  public String toString()
  {
    return "<" + var + ", " + context + ">";
  }
}
