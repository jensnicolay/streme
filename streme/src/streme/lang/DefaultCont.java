package streme.lang;

public abstract class DefaultCont<T> implements Cont<T>
{

  final private String name;

  public DefaultCont(String name)
  {
    super();
    this.name = name;
  }

  public final boolean equals(Object obj)
  {
    return (this == obj);
  }
  
  public String getName()
  {
    return name;
  }
  
  public String toString()
  {
    return "<defaultcont " + name + ">";
  }
}
