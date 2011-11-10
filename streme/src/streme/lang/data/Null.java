package streme.lang.data;

import java.lang.reflect.Array;
import java.util.Iterator;
import java.util.NoSuchElementException;

import streme.lang.StremeException;

public class Null extends Lst
{

  private static final Iterator<Object> ITER = new Iterator<Object>()
  {

    public void remove()
    {
      throw new UnsupportedOperationException();
    }

    public Object next()
    {
      throw new NoSuchElementException();
    }

    public boolean hasNext()
    {
      return false;
    }
  };
  //public static final Null NULL = new Null();
  private static final Object[] EMPTY = new Object[0];

  public Null()
  {
    super();
  }

  @Override
  public final boolean equals(Object obj)
  {
    return (obj instanceof Null);
  }

  @Override
  public final int hashCode()
  {
    return 19;
  }

  @Override
  public String toString()
  {
    return "()";
  }

  // @Override
  public Iterator<Object> iterator()
  {
    return ITER;
  }

  public Object assoc(Object object)
  {
    return Boolean.FALSE;
  }

  public final long length()
  {
    return 0;
  }
  
  public final boolean isNull()
  {
    return true;
  }


  @Override
  public Lst reverse()
  {
    return this;
  }

  @Override
  public Object listRef(int index)
  {
    throw new IndexOutOfBoundsException();
  }

  @Override
  public Object car()
  {
    throw new StremeException("car on empty list");
  }

  @Override
  public Object cdr()
  {
    throw new StremeException("cdr on empty list");
  }

  @Override
  public Lst append(Lst lst)
  {
    return lst;
  }

//  @Override
//  public void setCar(Object car)
//  {
//    throw new StremeException("set-car! on empty list");
//  }
//
//  @Override
//  public void setCdr(Object cdr)
//  {
//    throw new StremeException("set-cdr! on empty list");
//  }

  public Pair<Boolean, Object[]> toArray()
  {
    return Pair.cons(Boolean.TRUE, EMPTY);
  }
  
  public <T> Pair<Boolean, T[]> toArray(Class<T> type)
  {
    return Pair.cons(Boolean.TRUE, (T[]) Array.newInstance(type, 0));
  }
  
  public Object[] properToArray()
  {
    return EMPTY;
  }
  
  public <T> T[] properToArray(Class<T> type)
  {
    return (T[]) Array.newInstance(type, 0);
  }
  
  // @Override
  // public void evalSequence(StremeContext context, Cont cont, Env staticEnv, Env dynamicEnv)
  // {
  // throw new StremeException("empty body");
  // }
  //  
  // @Override
  // public void evalOperands(StremeContext context, PCont cont, Env env)
  // {
  // cont.call(NULL, env);
  // }
}
