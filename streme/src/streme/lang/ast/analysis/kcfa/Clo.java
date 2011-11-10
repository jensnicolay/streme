package streme.lang.ast.analysis.kcfa;

import java.util.Set;

import streme.lang.ast.Lambda;

public class Clo
{
  private Lambda lambda;
  private Benv benv;

  public Clo(Lambda lambda, Benv benv)
  {
    super();
    this.lambda = lambda;
    this.benv = benv;
  }

  public int hashCode()
  {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((benv == null) ? 0 : benv.hashCode());
    result = prime * result + ((lambda == null) ? 0 : lambda.hashCode());
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
    Clo other = (Clo) obj;
    if (benv == null)
    {
      if (other.benv != null)
        return false;
    }
    else if (!benv.equals(other.benv))
      return false;
    if (lambda == null)
    {
      if (other.lambda != null)
        return false;
    }
    else if (!lambda.equals(other.lambda))
      return false;
    return true;
  }

  public Lambda getLambda()
  {
    return lambda;
  }

  public Benv getBenv()
  {
    return benv;
  }
  
  public Set<Addr> touches()
  {
    return benv.touches();
  }

  public String toString()
  {
    return "[clo " + lambda.toShortString() + "]";
  }
}