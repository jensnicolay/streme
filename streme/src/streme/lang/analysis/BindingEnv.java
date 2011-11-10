package streme.lang.analysis;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import streme.lang.data.Sym;

public class BindingEnv
{
  private Map<Sym, AbstractVar<Time>> entries;

  public BindingEnv()
  {
    this(new HashMap<Sym, AbstractVar<Time>>());
  }
  
  public BindingEnv(BindingEnv benv)
  {
    this(new HashMap<Sym, AbstractVar<Time>>(benv.entries));
  }

  private BindingEnv(Map<Sym, AbstractVar<Time>> entries)
  {
    super();
    this.entries = entries;
  }

  public int hashCode()
  {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((entries == null) ? 0 : entries.hashCode());
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
    BindingEnv other = (BindingEnv) obj;
    if (entries == null)
    {
      if (other.entries != null)
        return false;
    }
    else if (!entries.equals(other.entries))
      return false;
    return true;
  }

  public AbstractVar<Time> lookup(Sym var)
  {
    AbstractVar<Time> abstractVar = entries.get(var);
    return abstractVar;
  }

  public void extend(Sym var, AbstractVar<Time> abstractVar)
  {
    entries.put(var, abstractVar);
  }
  
  public Set<AbstractVar<Time>> touches()
  {
    return new HashSet<AbstractVar<Time>>(entries.values());
  }

  public String toString()
  {
    return entries.toString() + "#" + Integer.toString(System.identityHashCode(this), 36);
  }
  
  public String toShortString()
  {
    return "#" + Integer.toString(System.identityHashCode(this), 36);
  }
}