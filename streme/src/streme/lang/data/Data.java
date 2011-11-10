package streme.lang.data;

import java.util.Comparator;


public final class Data
{
  public static final Comparator<Object> COMPARATOR = new Comparator<Object>()
  {
    public int compare(Object o1, Object o2)
    {
      if (o1 == o2)
      {
        return 0;
      }
      if (o1 == null)
      {
        return -1; 
      }
      if (o2 == null)
      {
        return 1;
      }
      if (o1.equals(o2))
      {
        return 0;
      }
      Class<? extends Object> c1 = o1.getClass();
      Class<? extends Object> c2 = o2.getClass();
      if (c1 != c2)
      {
        return c1.getName().compareTo(c2.getName());
      }
      return (((Comparable<Object>)o1).compareTo(o2));
    }
  };
  
  public static final String toString(Object object, String... meta)
  {
//    if (object instanceof IData)
//    {
//      return toString(((IData) object).toData(meta));
//    }
    if (Boolean.TRUE.equals(object))
    {
      return "#t";
    }
    if (Boolean.FALSE.equals(object))
    {
      return "#f";
    }
    if (object instanceof String)
    {
      return "\"" + object + "\"";
    }
    if (object == null)
    {
      return "<undefined>";
    }
    if (object == Void.TYPE)
    {
      return "<unspecified>";
    }
    if (object instanceof Object[])
    {
      Object[] vector = (Object[]) object;
      return "#(" + toStrings(vector, meta) + ")";
    }
    if (object instanceof Character)
    {
      return "#\\" + object;
    }
    return object.toString();
  }

  public static final String toStrings(Object[] exps, String... meta)
  {
    if (exps.length == 0)
    {
      return "";
    }
    StringBuilder sb = new StringBuilder();
    int i;
    for (i = 0; i < exps.length - 1; i++)
    {
      sb.append(toString(exps[i], meta)).append(" ");
    }
    sb.append(toString(exps[i], meta));
    return sb.toString();
  }
  
  public static final String toStringImproper(Object[] exps, String... meta)
  {
    StringBuilder sb = new StringBuilder();
    sb.append("(");
    for (int i = 0; i < exps.length - 1; i++)
    {
      sb.append(toString(exps[i], meta)).append(" ");
    }
    sb.append(". ");
    sb.append(toString(exps[exps.length - 1], meta));
    sb.append(")");
    return sb.toString();
  }

  
  private Data()
  {
    super();
  }
}
