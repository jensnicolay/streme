package streme.lang.data;

import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.IdentityHashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;

public class Pair<L, R> extends Lst
{
  public static final <L, R> Pair<L, R> cons(L car, R cdr)
  {
    return new Pair<L, R>(car, cdr);
  }

  private L car;
  private R cdr;

  public Pair(L car, R cdr)
  {
    super();
    this.car = car;
    this.cdr = cdr;
  }

  public int hashCode()
  {
    return hashCode(0);
  }

  public int hashCode(int depth)
  {
    if (depth > 64)
    {
      return 197;
    }
    final int prime = 31;
    int result = 1;
    result = prime * result
        + ((car == null) ? 0 : (car instanceof Pair ? ((Pair) car).hashCode(++depth) : car.hashCode()));
    result = prime * result
        + ((cdr == null) ? 0 : (cdr instanceof Pair ? ((Pair) cdr).hashCode(++depth) : cdr.hashCode()));
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
    Pair other = (Pair) obj;
    if (car == null)
    {
      if (other.car != null)
        return false;
    }
    else if (!car.equals(other.car))
      return false;
    if (cdr == null)
    {
      if (other.cdr != null)
        return false;
    }
    else if (!cdr.equals(other.cdr))
      return false;
    return true;
  }

  public Object assoc(Object object)
  {
    for (Object el : this)
    {
      Pair p = (Pair) el;
      if (object.equals(p.car()))
      {
        return p;
      }
    }
    return Boolean.FALSE;
  }

  public L car()
  {
    return car;
  }

  public R cdr()
  {
    return cdr;
  }

  public void setCar(L car)
  {
    this.car = car;
  }

  public void setCdr(R cdr)
  {
    this.cdr = cdr;
  }

  public String toString()
  {
    Map<Object, Integer> ags = new IdentityHashMap<Object, Integer>();
    return toStringInternal(ags);
  }

  public String toStringInternal(Map<Object, Integer> ags)
  {
    StringBuilder sb = new StringBuilder();
    sb.append("(");
    ags.put(this, ags.size());
    Pair p = this;
    do
    {
      Object car = p.car();
      if (car instanceof Pair)
      {
        Integer result = ags.get(car);
        if (result == null)
        {
          sb.append(((Pair) car).toStringInternal(ags));
        }
        else
        {
          sb.append("°" + result + "°");
        }
      }
      else
      {
        sb.append(Data.toString(car));
      }
      Object cdr = p.cdr();
      if (cdr instanceof Pair)
      {
        Integer result = ags.get(cdr);
        if (result == null)
        {
          p = (Pair) cdr;
          ags.put(p, ags.size());
          sb.append(" ");
          continue;
        }
        else
        {
          sb.append(" . °" + result + "°");
          break;
        }
      }
      else if (cdr instanceof Null)
      {
        break;
      }
      else
      {
        sb.append(" . ");
        sb.append(Data.toString(cdr));
        break;
      }
    }
    while (true);
    sb.append(")");
    return sb.toString();
  }

  public Iterator<Object> iterator()
  {
    return new Iterator<Object>()
    {
      Pair<L, R> p = Pair.this;

      public boolean hasNext()
      {
        return p != null;
      }

      public L next()
      {
        if (p == null)
        {
          throw new NoSuchElementException();
        }
        L next = p.car();
        R pcdr = p.cdr();
        if (pcdr instanceof Null)
        {
          p = null;
        }
        else if (pcdr instanceof Pair)
        {
          p = (Pair) pcdr;
        }
        else
        {
          throw new IllegalArgumentException("not a proper list");
        }
        return next;
      }

      public void remove()
      {
        throw new UnsupportedOperationException();
      }
    };
  }

  public <T> Pair<Boolean, T[]> toArray(Class<T> type)
  {
    Pair p = this;
    List<T> objects = new ArrayList<T>();
    objects.add((T) p.car());
    Object next = p.cdr();
    while (next instanceof Pair)
    {
      p = (Pair) next;
      objects.add((T) p.car());
      next = p.cdr();
    }
    if (next instanceof Null)
    {
      T[] array = (T[]) Array.newInstance(type, objects.size());
      return Pair.cons(Boolean.TRUE, objects.toArray(array));
    }
    else
    {
      objects.add((T) next);
      T[] array = (T[]) Array.newInstance(type, objects.size());
      return Pair.cons(Boolean.FALSE, objects.toArray(array));
    }
  }

  public Pair<Boolean, Object[]> toArray()
  {
    Pair p = this;
    List<Object> objects = new ArrayList<Object>();
    objects.add(p.car());
    Object next = p.cdr();
    while (next instanceof Pair)
    {
      p = (Pair) next;
      objects.add(p.car());
      next = p.cdr();
    }
    if (next instanceof Null)
    {
      return Pair.cons(Boolean.TRUE, objects.toArray());
    }
    else
    {
      objects.add(next);
      return Pair.cons(Boolean.FALSE, objects.toArray());
    }
  }

  public Object[] properToArray()
  {
    Pair p = this;
    List<Object> objects = new ArrayList<Object>();
    objects.add(p.car());
    Object next = p.cdr();
    while (!(next instanceof Null))
    {
      p = (Pair) next;
      objects.add(p.car());
      next = p.cdr();
    }
    return objects.toArray();
  }

  public <T> T[] properToArray(Class<T> type)
  {
    Lst p = this;
    List<T> objects = new ArrayList<T>();
    objects.add((T) p.car());
    Lst next = (Lst) p.cdr();
    while (!next.isNull())
    {
      p = next;
      objects.add((T) p.car());
      next = (Lst) p.cdr();
    }
    return objects.toArray((T[]) Array.newInstance(type, objects.size()));
  }

  public long length()
  {
    int length = 0;
    for (@SuppressWarnings("unused")
    Object _el : this)
    {
      length++;
    }
    return length;
  }

  public final boolean isNull()
  {
    return false;
  }

  public Lst append(Lst lst)
  {
    Lst next = (Lst) cdr();
    Pair firstP = cons(car(), next);
    Pair lastP = firstP;
    while (!next.isNull())
    {
      lastP.setCdr(lastP = cons(next.car(), ((next = (Lst) next.cdr()))));
    }
    lastP.setCdr(lst);
    return firstP;
  }

  public Lst reverse()
  {
    Lst l = new Null();
    for (Object el : this)
    {
      l = cons(el, l);
    }
    return l;
  }

  public Object listRef(int index)
  {
    Pair ag = this;
    for (int i = 0; i < index; i++)
    {
      ag = (Pair) ag.cdr();
    }
    return ag.car();
  }
}
