package streme.lang.ast.analysis.kcfa;

import streme.lang.data.IData;
import streme.lang.data.Lst;
import streme.lang.data.Sym;

public class Binding implements Addr, IData
{
  private Sym var;
  private Time time;

  public Binding(Sym var, Time time)
  {
    super();
    this.var = var;
    this.time = time;
  }

  public int hashCode()
  {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((time == null) ? 0 : time.hashCode());
    result = prime * result + ((var == null) ? 0 : var.hashCode());
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
    Binding other = (Binding) obj;
    if (time == null)
    {
      if (other.time != null)
        return false;
    }
    else if (!time.equals(other.time))
      return false;
    if (var == null)
    {
      if (other.var != null)
        return false;
    }
    else if (!var.equals(other.var))
      return false;
    return true;
  }

  public int compareTo(Addr o)
  {
    int classCompare = getClass().getName().compareTo(o.getClass().getName());
    if (classCompare != 0)
    {
      return classCompare;
    }
    Binding other = (Binding) o;
    int cvar = var.compareTo(other.var);
    if (cvar != 0)
    {
      return cvar;
    }
    return time.compareTo(other.time);
  }

  public Sym getVar()
  {
    return var;
  }

  public Time getTime()
  {
    return time;
  }

  public Lst toData()
  {
    return Lst.valueOf(new Sym("binding"), getVar(), getTime().toData());
  }

  public String toString()
  {
    return toData().toString();
  }
}