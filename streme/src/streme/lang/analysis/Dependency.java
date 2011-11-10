package streme.lang.analysis;

import java.util.Set;

public class Dependency<T>
{
  public enum Type
  {
    R, W, RW, WR, WW
  };

  public enum Source
  {
    LEX, IP, VAR
  }

  private Type type;
  private Source source;
  private T target;

  public Dependency(Type type, Source source, T target)
  {
    super();
    this.type = type;
    this.source = source;
    this.target = target;
  }

  public int hashCode()
  {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((source == null) ? 0 : source.hashCode());
    result = prime * result + ((target == null) ? 0 : target.hashCode());
    result = prime * result + ((type == null) ? 0 : type.hashCode());
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
    Dependency other = (Dependency) obj;
    if (source != other.source)
      return false;
    if (target == null)
    {
      if (other.target != null)
        return false;
    }
    else if (!target.equals(other.target))
      return false;
    if (type != other.type)
      return false;
    return true;
  }

  public Type getType()
  {
    return type;
  }

  public Source getSource()
  {
    return source;
  }

  public T getTarget()
  {
    return target;
  }

  public String toString()
  {
    return "(dep " + type + " " + source + ": " + target + ")";
  }
}
