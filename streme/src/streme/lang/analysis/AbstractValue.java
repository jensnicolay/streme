package streme.lang.analysis;

import java.util.HashSet;
import java.util.Set;

public class AbstractValue
{
  private Set<Object> values;
  private Set<Object> mono;
  private boolean fresh;

  public AbstractValue(AbstractValue v)
  {
    this(v.values, v.fresh, v.mono);
  }

  public AbstractValue(Set<Object> d)
  {
    this(d, true, d);
  }

  private AbstractValue(Set<Object> d, boolean fresh, Set<Object> mono)
  {
    super();
    this.values = new HashSet<Object>(d);
    this.fresh = fresh;
    this.mono = new HashSet<Object>(mono);
  }

  public boolean subsumes(AbstractValue other)
  {
    return other.values.containsAll(values);
  }

  public <T> Set<T> values(Class<T> type)
  {
    return (Set<T>) new HashSet<Object>(values);
  }

  public <T> Set<T> mono(Class<T> type)
  {
    return (Set<T>) new HashSet<Object>(mono);
  }

  private void strongUpdate(Set<Object> d)
  {
    this.values = new HashSet<Object>(d);
    mono.addAll(d);
    // LOGGER.finer("strong update " + d + " ==> " + this);
  }

  public void weakUpdate(Set<Object> d)
  {
    this.values.addAll(d);
    mono.addAll(d);
    fresh = false;
    // LOGGER.finer("weak update " + d + " ==> " + this);
  }
  
  public void reset()
  {
    values.clear();
    fresh = true;
  }

  public void update(Set<Object> d)
  {
    if (fresh)
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
    return "[" + (fresh ? "#1 " : "") + values + "]";
  }
}
