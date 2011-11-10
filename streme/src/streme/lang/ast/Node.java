package streme.lang.ast;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;

import streme.lang.data.Data;
import streme.lang.data.IData;
import streme.lang.data.Lst;
import streme.lang.data.Null;

public abstract class Node implements IData, Comparable<Node>
{
  
  private static final AtomicInteger nodeTagger = new AtomicInteger();
  
  public static boolean nodeEquals(Node[] ns1, Node[] ns2)
  {
    if (ns1.length != ns2.length)
    {
      return false;
    }
    for (int i = 0; i < ns1.length; i++)
    {
      if (!ns1[i].nodeEquals(ns2[i]))
      {
        return false;
      }
    }
    return true;
  }
  
  public static int nodeCompareSafe(Node n1, Node n2)
  {
    if (n1 == null)
    {
      if (n2 == null)
      {
        return 0;
      }
      return -1;
    }
    else if (n2 == null)
    {
      return 1;
    }
    return n1.compareTo(n2);
  }
  
  public enum Type
  {
    APPLICATION, BEGIN, DEFINE, IF, LAMBDA, LET, SETVAR, VAR, FUTURE, REF, BINDING, LITERAL
  }

  private int tag = nodeTagger.incrementAndGet();
  private Map<String, Object> properties;
  
  public final int getTag()
  {
    return tag;
  }
  
  public final int hashCode()
  {
    return tag;
  }
  
  public final int compareTo(Node o)
  {
    return tag - o.tag;
  };
  
  public abstract Type type();

  public Object getProperty(String name)
  {
    return getProperty(name, null);
  }

  public final <T> T getProperty(String name, T deflt)
  {
    if (properties == null)
    {
      setProperty(name, deflt);
      return deflt;
    }
    T object = (T) properties.get(name);
    if (object == null)
    {
      setProperty(name, deflt);
      return deflt;
    }
    return object;
  }

  public final void setProperty(String name, Object value)
  {
    if (properties == null)
    {
      properties = new HashMap<String, Object>();
    }
    properties.put(name, value);
  }
  
  public final void resetProperty(String name)
  {
    if (properties != null)
    {
      properties.put(name, null);
    }
  }
  
  public final void putAllProperties(Node source)
  {
    if (properties == null)
    {
      properties = new HashMap<String, Object>(source.properties);
    }
    else
    {
      properties.putAll(source.properties);
    }
  }
  
//  public final boolean addPropertyToSet(String name, Object value)
//  {
//    Set<Object> set = (Set<Object>) getProperty(name);
//    if (value == null)
//    {
//      set = new HashSet<Object>();
//      setProperty(name, set);
//    }
//    return set.add(value);
//  }
//  
  public String toShortString()
  {
    return toString();
  }
  
  protected String shorten(String s)
  {
    if (s.length() > 16)
    {
      if (s.startsWith("("))
      {
        return s.substring(0, 16) + "...)";
      }
      return s.substring(0, 16) + "...";
    }
    return s;
  }
  
  public final String toString()
  {
    return Data.toString(toData());
  }
  
  public final void visitChildren(AstVisitor visitor)
  {
    for (Object node : children())
    {
      ((Node) node).accept(visitor);
    }
  }
    
  public abstract void accept(AstVisitor visitor);
  public abstract boolean nodeEquals(Node node);
  public abstract Object toData();
  public abstract Lst children();
  public abstract Node fromChildren(Lst nodes);
  
  public String toStringWithMeta(String... meta)
  {
    StringBuilder sb = new StringBuilder();
    boolean hasMeta = false;
    for (String s : meta)
    {
      Object value = getProperty(s);
      if (value != null)
      {
        if (!hasMeta)
        {
          sb.append("^(");
          hasMeta = true;
          sb.append("(").append(s).append(" ").append(value).append(")");
        }
        else
        {
          sb.append(" (").append(s).append(" ").append(value).append(")");
        }
      }
    }
    if (hasMeta)
    {
      sb.append(") ").append(toString());
      return sb.toString();
    }
    return toString();
  }
}
