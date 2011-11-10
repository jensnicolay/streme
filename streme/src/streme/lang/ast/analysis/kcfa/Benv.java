package streme.lang.ast.analysis.kcfa;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import streme.lang.data.Sym;

public class Benv
{
  private Map<Sym, Addr> entries;

  public Benv()
  {
    this(new HashMap<Sym, Addr>());
  }
  
  public Benv(Benv benv)
  {
    this(new HashMap<Sym, Addr>(benv.entries));
  }

  private Benv(Map<Sym, Addr> entries)
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
    Benv other = (Benv) obj;
    if (entries == null)
    {
      if (other.entries != null)
        return false;
    }
    else if (!entries.equals(other.entries))
      return false;
    return true;
  }

  public Addr lookup(Sym var)
  {
    Addr addr = entries.get(var);
    return addr;
  }

  public void extend(Sym var, Addr addr)
  {
    entries.put(var, addr);
  }
  
  public Set<Addr> touches()
  {
    return new HashSet<Addr>(entries.values());
  }

  public String toString()
  {
    return entries.toString();
  }
}