package streme.lang.data;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;

public abstract class Lst implements Iterable<Object>
{
  public interface Mapper
  {
    Object map(Object car);
  }

  public static Lst valueOf(Object... ag)
  {
    Lst l = new Null();
    for (int i = ag.length - 1; i > -1; i--)
    {
      l = Pair.cons(ag[i], l);
    }
    return l;
  }

  public static Lst valueOfImproper(Object... ag)
  {
    Lst l = Pair.cons(ag[ag.length - 2], ag[ag.length - 1]);
    for (int i = ag.length - 3; i > -1; i--)
    {
      l = Pair.cons(ag[i], l);
    }
    return l;
  }

  public static Lst valueOf(Collection<? extends Object> collection)
  {
    return valueOf(collection.toArray());
  }
  
  public static Lst valueOfImproper(Collection<? extends Object> collection)
  {
    return valueOfImproper(collection.toArray());
  }
  
//  public static Lst valueOf(ArrayList list)
//  {
//    Lst l = Null.NULL;
//    for (int i = list.size() - 1; i >= 0; i--)
//    {
//      l = Pair.cons(list.get(i), l);
//    }
//    return l;
//  }
  
  public static Lst assocValueOf(Map<?, ? extends Object> map)
  {
    Lst l = new Null();
    for (Map.Entry<?, ?> entry : map.entrySet())
    {
      l = Pair.cons(Pair.cons(entry.getKey(), entry.getValue()), l);
    }
    return l;
  }

  public Lst map(Mapper mapper)
  {
    List<Object> rewritten = new ArrayList<Object>();
    Lst l = this;
    while (!l.isNull())
    {
      Object r = mapper.map(l.car());
      rewritten.add(r);
      l = (Lst) l.cdr();
    }
    return valueOf(rewritten);
  }

  public Lst mapAtoms(Mapper mapper)
  {
    Lst rewritten = new Null();
    Lst current = this;
    while (!current.isNull())
    {
      if (current instanceof Pair)
      {
        Pair c = (Pair) current;
        Object car = c.car();
        if (car instanceof Lst)
        {
          rewritten = Pair.cons(((Lst) car).mapAtoms(mapper), rewritten);
        }
        else
        {
          rewritten = Pair.cons(mapper.map(car), rewritten);
        }
        Object cdr = c.cdr();
        if (cdr instanceof Null)
        {
          return rewritten.reverse();
        }
        if (cdr instanceof Pair)
        {
          rewritten = rewritten.reverse();
          ((Pair) rewritten).setCdr(mapper.map(cdr));
          return rewritten;
        }
        else
        {
          current = (Lst) cdr;
        }
      }
    }
    return rewritten.reverse();
  }

  public Lst zip(Lst b)
  {
    Lst a = this;
    List<Object> result = new ArrayList<Object>();
    while (!a.isNull() && !b.isNull())
    {
      result.add(Pair.cons(a.car(), Pair.cons(b.car(), new Null())));
      a = (Lst) a.cdr();
      b = (Lst) b.cdr();
    }
    return Lst.valueOf(result);
  }

  public abstract boolean isNull();
  
  public abstract Object assoc(Object object);

  public abstract long length();

  public abstract Lst reverse();

  public abstract Object listRef(int index);

  public abstract Object car();

  public abstract Object cdr();

//  public abstract void setCar(Object car);
//
//  public abstract void setCdr(Object cdr);

  public Lst listTail(int k)
  {
    Lst l = this;
    while (k > 0)
    {
      l = (Lst) l.cdr();
      k--;
    }
    return l;
  }
  
  public Lst take(int k)
  {
    Lst l = this;
    List<Object> result = new ArrayList<Object>();
    while (k-- > 0 && !l.isNull())
    {
      result.add(l.car());
      l = (Lst) l.cdr();
    }
    return Lst.valueOf(result);
  }

  public Object cddr()
  {
    return ((Lst) cdr()).cdr();
  }

  public Object caar()
  {
    return ((Lst) car()).car();
  }

  public Object caddr()
  {
    return ((Lst) cddr()).car();
  }

  public Object cadr()
  {
    return ((Lst) cdr()).car();
  }

  public Object cdar()
  {
    return ((Lst) car()).cdr();
  }

  public Object cadaar()
  {
    return ((Lst) cdaar()).car();
  }

  public Object cdaar()
  {
    return ((Lst) caar()).cdr();
  }

  public abstract Lst append(Lst lst);

  public abstract Pair<Boolean, Object[]> toArray();
  public abstract <T> Pair<Boolean, T[]> toArray(Class<T> type);

  public abstract Object[] properToArray();
  public abstract <T> T[] properToArray(Class<T> type);

  public Object cadddr()
  {
    return ((Lst) cdddr()).car();
  }

  public Object cdddr()
  {
    return ((Lst) cddr()).cdr();
  }

  public Object cddddr()
  {
    return ((Lst) cdddr()).cdr();
  }

  public Object cadadr()
  {
    return ((Lst) cdadr()).car();
  }

  public Object cdadr()
  {
    return ((Lst) cadr()).cdr();
  }

  public Object caadr()
  {
    return ((Lst) cadr()).car();
  }

  public static void main(String[] args)
  {
    Lst l1 = Lst.valueOf("1", "2", "3");
    Lst l2 = Lst.valueOf("a", "b", "c", "d");
    System.out.println(l2.take(0));
    System.out.println(l2.take(1));
    System.out.println(l2.take(2));
    System.out.println(l2.take(3));
    System.out.println(l2.take(4));
  }
}
