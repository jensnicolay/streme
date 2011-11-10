package streme.lang.data;


public class Sym implements Comparable<Sym>
{

  public static final String getName(Object object)
  {
    if (object instanceof Sym)
    {
      return ((Sym) object).getName();
    }
    return null;
  }
  
  private String name;

//  public static final Sym get(String name)
//  {
//    Sym symb = TABLE.get(name);
//    if (symb == null)
//    {
//      symb = new Sym(name);
//      TABLE.put(name, symb);
//    }
//    return symb;
//  }

  public Sym(String name)
  {
    super();
    this.name = name.intern();
  }

  public String getName()
  {
    return name;
  }

  public final int hashCode()
  {
    return name.hashCode();
  }

  public final boolean equals(Object obj)
  {
    return getName(obj) == name;
  }

  public String toString()
  {
    return name;
  }
  
  public int compareTo(Sym o)
  {
    return name.compareTo(o.getName());
  }
}
