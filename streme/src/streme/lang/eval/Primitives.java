package streme.lang.eval;

import java.math.BigInteger;

import streme.lang.StremeException;
import streme.lang.data.BigRational;
import streme.lang.data.Null;
import streme.lang.data.Pair;
import streme.lang.data.Sym;

public class Primitives
{
  
  public static final boolean evenp(Object operand)
  {
    return ((BigInteger) operand).mod(new BigInteger("2")).equals(BigInteger.ZERO);
  }
  
  public static final boolean oddp(Object operand)
  {
    return ((BigInteger) operand).mod(new BigInteger("2")).equals(BigInteger.ONE);
  }
  
  public static final boolean eq(Object operand1, Object operand2)
  {
    if (operand1 instanceof Null || operand1 instanceof Sym || operand1 instanceof Boolean || operand1 instanceof Number || operand1 instanceof BigInteger)
    {
      return operand1.equals(operand2);
    }
    return operand1 == operand2;
  }
  
  public static final Object memq(Object candidate, Object list)
  {
    while (!(list instanceof Null))
    {
      Pair p = (Pair) list;
      if (eq(candidate, p.car()))
      {
        return p;
      }
      list = p.cdr();
    }
    return false;
  }

  public static final boolean eqv(Object operand1, Object operand2)
  {
    if (operand1 instanceof Null || operand1 instanceof Sym || operand1 instanceof Boolean || operand1 instanceof Number || operand1 instanceof BigInteger)
    {
      return operand1.equals(operand2);
    }
    return operand1 == operand2;
  }
  
  public static final Object memv(Object candidate, Object list)
  {
    while (!(list instanceof Null))
    {
      Pair p = (Pair) list;
      if (eqv(candidate, p.car()))
      {
        return p;
      }
      list = p.cdr();
    }
    return false;
  }

  public static final boolean equal(Object o1, Object o2)
  {
    if (o1 instanceof Pair)
    {
      if (o2 instanceof Pair)
      {
        Pair p1 = (Pair) o1;
        Pair p2 = (Pair) o2;
        return equal(p1.car(), p2.car()) && equal(p1.cdr(), p2.cdr());
      }
      else
      {
        return false;
      }
    }
    if (o1 instanceof String)
    {
      return o1.equals(o2);
    }
    return eqv(o1, o2);
  }
  
  public static final Object member(Object candidate, Object list)
  {
    while (!(list instanceof Null))
    {
      Pair p = (Pair) list;
      if (equal(candidate, p.car()))
      {
        return p;
      }
      list = p.cdr();
    }
    return false;
  }

  public static Object plus(Object operand1, Object operand2)
  {
    Class c1 = operand1.getClass();
    Class c2 = operand2.getClass();
    if (c1 == Integer.class)
    {
      if (c2 == Integer.class)
      {
        int i = (Integer) operand1;
        int j = (Integer) operand2;
        return i + j;
      }
    }
    if (c1 == Double.class)
    {
      if (c2 == Double.class)
      {
        double i = (Double) operand1;
        double j = (Double) operand2;
        return i + j;
      }
    }
    if (c1 == Long.class)
    {
      if (c2 == Long.class)
      {
        long i = (Long) operand1;
        long j = (Long) operand2;
        return i + j;
      }
    }
    throw new IllegalArgumentException("(+ " + operand1 + " " + operand2 + ")");
  }

  public static Object multiply(Object operand1, Object operand2)
  {
    Class c1 = operand1.getClass();
    Class c2 = operand2.getClass();
    if (c1 == Integer.class)
    {
      if (c2 == Integer.class)
      {
        int i = (Integer) operand1;
        int j = (Integer) operand2;
        return i * j;
      }
    }
    if (c1 == Double.class)
    {
      if (c2 == Double.class)
      {
        double i = (Double) operand1;
        double j = (Double) operand2;
        return i * j;
      }
    }
    if (c1 == Long.class)
    {
      if (c2 == Long.class)
      {
        long i = (Long) operand1;
        long j = (Long) operand2;
        return i * j;
      }
    }
    throw new IllegalArgumentException("(* " + operand1 + " " + operand2 + ")");
  }

  public static Object minus(Object operand1, Object operand2)
  {
    Class c1 = operand1.getClass();
    Class c2 = operand2.getClass();
    if (c1 == Integer.class)
    {
      if (c2 == Integer.class)
      {
        int i = (Integer) operand1;
        int j = (Integer) operand2;
        return i - j;
      }
    }
    if (c1 == Double.class)
    {
      if (c2 == Double.class)
      {
        double i = (Double) operand1;
        double j = (Double) operand2;
        return i - j;
      }
    }
    if (c1 == Long.class)
    {
      if (c2 == Long.class)
      {
        long i = (Long) operand1;
        long j = (Long) operand2;
        return i - j;
      }
    }
    throw new IllegalArgumentException("(- " + operand1 + " " + operand2 + ")");
  }

  public static Object minus(Object operand)
  {
    Class c = operand.getClass();
    if (c == Integer.class)
    {
      int i = (Integer) operand;
      return -i;
    }
    if (c == Double.class)
    {
      double i = (Double) operand;
      return -i;
    }
    if (c == Long.class)
    {
      long i = (Long) operand;
      return -i;
    }
    throw new IllegalArgumentException("(- " + operand + ")");
  }

  public static Object divide(Object operand)
  {
    Class c = operand.getClass();
    if (c == Integer.class)
    {
      int i = (Integer) operand;
      return 1 / i;
    }
    if (c == Double.class)
    {
      double i = (Double) operand;
      return 1 / i;
    }
    if (c == Long.class)
    {
      long i = (Long) operand;
      return 1 / i;
    }
    throw new IllegalArgumentException("(- " + operand + ")");
  }

  public static Object divide(Object operand1, Object operand2)
  {
    Class c1 = operand1.getClass();
    Class c2 = operand2.getClass();
    if (c1 == Integer.class)
    {
      if (c2 == Integer.class)
      {
        int i = (Integer) operand1;
        int j = (Integer) operand2;
        return i / j;
      }
    }
    if (c1 == Double.class)
    {
      if (c2 == Double.class)
      {
        double i = (Double) operand1;
        double j = (Double) operand2;
        return i / j;
      }
    }
    if (c1 == Long.class)
    {
      if (c2 == Long.class)
      {
        long i = (Long) operand1;
        long j = (Long) operand2;
        return i / j;
      }
    }
    throw new IllegalArgumentException("(/ " + operand1 + " " + operand2 + ")");
  }

  public static Object max(Object operand1, Object operand2)
  {
    Class c1 = operand1.getClass();
    Class c2 = operand2.getClass();
    if (c1 == Integer.class)
    {
      if (c2 == Integer.class)
      {
        int i = (Integer) operand1;
        int j = (Integer) operand2;
        return Math.max(i, j);
      }
    }
    if (c1 == Double.class)
    {
      if (c2 == Double.class)
      {
        double i = (Double) operand1;
        double j = (Double) operand2;
        return Math.max(i, j);
      }
    }
    if (c1 == Long.class)
    {
      if (c2 == Long.class)
      {
        long i = (Long) operand1;
        long j = (Long) operand2;
        return Math.max(i, j);
      }
    }
    throw new IllegalArgumentException("(max " + operand1 + " " + operand2 + ")");
  }

  public static Class<?> classForName(Object operand)
  {
    String name = String.valueOf(operand);
    Class<?> clazz;
    if ("String".equals(name))
    {
      clazz = String.class;
    }
    else if ("Double".equals(name))
    {
      clazz = Double.class;
    }
    else if ("Integer".equals(name))
    {
      clazz = Integer.class;
    }
    else if ("Long".equals(name))
    {
      clazz = Long.class;
    }
    else if ("Void".equals(name))
    {
      clazz = Void.class;
    }
    else if ("Math".equals(name))
    {
      clazz = Math.class;
    }
    else if ("System".equals(name))
    {
      clazz = System.class;
    }
    else if ("Runnable".equals(name))
    {
      clazz = Runnable.class;
    }
    else
    {
      try
      {
        clazz = Class.forName(name);
      }
      catch (ClassNotFoundException e)
      {
        throw new StremeException("reflection error", e);
      }
    }
    return clazz;
  }

  public static Object stringLtP(Object operand1, Object operand2)
  {
    return ((String) operand1).compareTo((String) operand2) < 0;
  }

  public static Object stringLteP(Object operand1, Object operand2)
  {
    return ((String) operand1).compareTo((String) operand2) <= 0;
  }

  public static Object stringGtP(Object operand1, Object operand2)
  {
    return ((String) operand1).compareTo((String) operand2) > 0;
  }

  public static Object stringGteP(Object operand1, Object operand2)
  {
    return ((String) operand1).compareTo((String) operand2) >= 0;
  }

  public static Object error(Object message)
  {
    throw new StremeException(message.toString());
  }

  private Primitives()
  {
    super();
  }
}
