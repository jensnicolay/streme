package streme.lang.ast.analysis.ipda;

import streme.lang.ast.Node;
import streme.lang.ast.analysis.kcfa.Addr;
import streme.lang.ast.analysis.kcfa.Time;

public class RetPoint implements Addr
{
  private Node exp;
  private Time time;
  
  public RetPoint(Node exp, Time time)
  {
    super();
    this.exp = exp;
    this.time = time;
  }

  public int hashCode()
  {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((exp == null) ? 0 : exp.hashCode());
    result = prime * result + ((time == null) ? 0 : time.hashCode());
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
    RetPoint other = (RetPoint) obj;
    if (exp == null)
    {
      if (other.exp != null)
        return false;
    }
    else if (!exp.equals(other.exp))
      return false;
    if (time == null)
    {
      if (other.time != null)
        return false;
    }
    else if (!time.equals(other.time))
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
    RetPoint other = (RetPoint) o;
    int cexp = Node.nodeCompareSafe(exp, other.exp);
    if (cexp != 0)
    {
      return cexp;
    }
    return time.compareTo(other.time);
  }
  
  public Node getExp()
  {
    return exp;
  }

  public String toString()
  {
    return "rp(" + (exp == null ? "null" : exp.toShortString()) + " " + time + ")";
  }
  
}
