package streme.lang.ast.analysis.kcfa;

import java.util.logging.Logger;

import streme.lang.data.IData;
import streme.lang.data.Lst;
import streme.lang.data.Null;
import streme.lang.data.Pair;

public class Time implements IData, Comparable<Time>
{
  public static final Logger LOGGER = Logger.getLogger("time");
  
  private Lst t;

  public Time()
  {
    this(new Null());
  }

  public Time(Lst t)
  {
    super();
    this.t = t;
  }

  public int hashCode()
  {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((t == null) ? 0 : t.hashCode());
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
    Time other = (Time) obj;
    if (t == null)
    {
      if (other.t != null)
        return false;
    }
    else if (!t.equals(other.t))
      return false;
    return true;
  }

  public int compareTo(Time time2)
  {
    Lst t1 = t;
    Lst t2 = time2.t;
    while (!t1.isNull() || !t2.isNull())
    {
      if (t1.isNull())
      {
        return -1;
      }
      if (t2.isNull())
      {
        return 1;
      }
      Comparable<Object> car1 = (Comparable<Object>) t1.car();
      Comparable<Object> car2 = (Comparable<Object>) t2.car();
      int c = car1.compareTo(car2);
      if (c != 0)
      {
        return c;
      }
      t1 = (Lst) t1.cdr();
      t2 = (Lst) t2.cdr();
    }
    return 0;
  }

  public Time tick(Object item, int k)
  {
    Time time = new Time(Pair.cons(item, t).take(k));
    LOGGER.fine(time.toString());
    return time;
  }
  
  public Lst toData()
  {
    return t;
  }

  public String toString()
  {
    return toData().toString();
  }
  
  public static void main(String[] args)
  {
    Time t1 = new Time();
    t1 = t1.tick(45, 3);
    t1 = t1.tick(45, 3);
    t1 = t1.tick(45, 3);
    Time t2 = new Time();
    t2 = t2.tick(45, 3);
    t2 = t2.tick(45, 3);
    t2 = t2.tick(45, 3);
    System.out.println(t1 +  " " + t2);
    System.out.println(t1.equals(t2));
  }
}