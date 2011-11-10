package streme.lang.ast.analysis.kcfa;

import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import streme.lang.StremeException;
import streme.lang.data.Sym;

public class Store
{
  public static final Logger LOGGER = Logger.getLogger("store");
 
  private enum Cardinality
  {
    ZERO, ONE, MANY
  };

  private static class StoreValue
  {
    private Set<Object> d;
    private Cardinality cardinality;
   
    public StoreValue(StoreValue v)
    {
      this(v.d, v.cardinality);
    }

    public StoreValue(Set<Object> d)
    {
      this(d, Cardinality.ONE);
    }

    private StoreValue(Set<Object> d, Cardinality cardinality)
    {
      super();
      this.d = new HashSet<Object>(d);
      this.cardinality = cardinality;
    }
   
    public int hashCode()
    {
      final int prime = 31;
      int result = 1;
      result = prime * result + ((d == null) ? 0 : d.hashCode());
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
      StoreValue other = (StoreValue) obj;
      if (d == null)
      {
        if (other.d != null)
          return false;
      }
      else if (!d.equals(other.d))
        return false;
      return true;
    }
   
    public boolean subsumes(StoreValue other)
    {
      return other.d.containsAll(d);
    }

    public <T> Set<T> d(Class<T> type)
    {
      return Collections.unmodifiableSet((Set<T>) d);
    }
   
    public void strongUpdate(Set<Object> d)
    {
      this.d = new HashSet<Object>(d);
      LOGGER.finer("strong update " + d  + " ==> " + this);
    }   

    public void weakUpdate(Set<Object> d)
    {
      this.d.addAll(d);
      cardinality = Cardinality.MANY;
      LOGGER.finer("weak update " + d  + " ==> " + this);  
    }

    public void update(Set<Object> d)
    {
      if (cardinality == Cardinality.ONE)
      {
        strongUpdate(d);
      }
      else
      {
        weakUpdate(d);
      }
    }
   
    public String toString()
    {
      return "[" + cardinality + " " + d + "]";
    }
  }

  private Map<Addr, StoreValue> entries;

  public Store()
  {
    this(Collections.<Addr, StoreValue>emptyMap());
  }

  public Store(Store store)
  {
    this(store.entries);
  }

  private Store(Map<Addr, StoreValue> entries)
  {
    super();
    this.entries = new HashMap<Addr, StoreValue>();
    for (Map.Entry<Addr, StoreValue> entry : entries.entrySet())
    {
      this.entries.put(entry.getKey(), new StoreValue(entry.getValue()));
    }
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
    Store other = (Store) obj;
    if (entries == null)
    {
      if (other.entries != null)
        return false;
    }
    else if (!entries.equals(other.entries))
      return false;
    return true;
  }

  public boolean subsumes(Store other)
  {
    for (Addr a : entries.keySet())
    {
      StoreValue sv1 = entries.get(a);
      StoreValue sv2 = other.entries.get(a);
      if (sv2 == null)
      {
        return false;
      }
      if (!sv1.subsumes(sv2))
      {
        return false;
      }
    }
    return true;
  }

  public void insert(Addr addr, Set<Object> d)
  {
    StoreValue value = entries.get(addr);
    if (value == null)
    {
      value = new StoreValue(d);
      entries.put(addr, value);
      LOGGER.finer("new entry " + addr + "->" + value);
    }
    else
    {
      LOGGER.finer("existing entry " + addr + "->" + value);
      value.weakUpdate(d);
    }
  }

  public Set<Object> safeLookup(Addr addr)
  {
    StoreValue value = entries.get(addr);
    if (value == null)
    {
      return null;
    }
    return value.d(Object.class);
  }

  public <T> Set<T> lookup(Addr addr, Class<T> type)
  {
    StoreValue value = entries.get(addr);
    if (value == null)
    {
      throw new StremeException("address not found: " + addr);
    }
    return value.d(type);
  }

  public void update(Addr addr, Set<Object> d)
  {
    StoreValue value = entries.get(addr);
    if (value == null)
    {
      throw new StremeException("address not found: " + addr);
    }
    LOGGER.finer("updating " + addr + "->" + value);
    value.update(d);
  }
 
  public void strongUpdate(Addr addr, Set<Object> d)
  {
    StoreValue value = entries.get(addr);
    if (value == null)
    {
      value = new StoreValue(d);
      entries.put(addr, value);
      LOGGER.fine("strong updating new entry " + addr + "->" + value);
    }
    else
    {
      LOGGER.fine("strong updating existing entry " + addr + "->" + value);
      value.update(d);
    }
  }

  public void join(Store store)
  {
    for (Entry<Addr, StoreValue> entry : store.entries.entrySet())
    {
      Addr addr = entry.getKey();
      StoreValue otherValue = entry.getValue();
      Set<Object> otherD = otherValue.d(Object.class);
      StoreValue value = entries.get(addr);
      if (value == null)
      {
        value = new StoreValue(otherD);
        entries.put(addr, value);
      }
      else
      {
        if (value.cardinality == Cardinality.ONE && otherValue.cardinality == Cardinality.ONE)
        {
          Set<Object> newD = new HashSet<Object>(otherD);
          newD.addAll(value.d(Object.class));
          value.strongUpdate(newD);
        }
        else
        {
          value.weakUpdate(otherD);
        }
      }
    }
  }

  public Map<Sym, Set<Object>> monovariant()
  {
    Map<Sym, Set<Object>> monoStore = new HashMap<Sym, Set<Object>>();
    for (Entry<Addr, StoreValue> entry : entries.entrySet())
    {
      Addr addr = entry.getKey();
      if (!(addr instanceof Binding))
      {
        continue;
      }
      Sym var = ((Binding) addr).getVar();
      Set<Object> values = entry.getValue().d(Object.class);
      Set<Object> monoValues = new HashSet<Object>();
      for (Object value : values)
      {
        if (value instanceof Clo)
        {
          monoValues.add(((Clo) value).getLambda());
        }
        else
        {
          monoValues.add(value);
        }
      }
      Set<Object> currentValues = monoStore.get(var);
      if (currentValues == null)
      {
        monoStore.put(var, monoValues);
      }
      else
      {
        currentValues.addAll(monoValues);
      }
    }
    return monoStore;
  }
 
  public void narrow(Set<Addr> keep)
  {
    if (LOGGER.isLoggable(Level.FINE))
    {
      Set<Addr> removing = new HashSet<Addr>(entries.keySet());
      removing.removeAll(keep);
      LOGGER.fine("removing " + removing + " from " + entries.keySet());
    }
    entries.keySet().retainAll(keep);
  }
 
  public String toString()
  {
    return entries.keySet().toString();
  }

  public String toStringNice()
  {
    StringBuilder sb = new StringBuilder();
    for (Entry<Addr, StoreValue> entry : entries.entrySet())
    {
      sb.append(entry).append('\n');
    }
    return sb.toString();
  }
 
  public static void main(String[] args)
  {
    Store st1 = new Store();
    Store st2 = new Store();
    Addr a = new Binding(new Sym("p"), new Time());
    Set<Object> s1 = new HashSet<Object>();
    s1.add(1);
    Set<Object> s2 = new HashSet<Object>();
    s2.add(1);
    s2.add(2);
    st1.insert(a, s1);
    st2.insert(a, s2);
    System.out.println(st1.subsumes(st2));
  }
} 