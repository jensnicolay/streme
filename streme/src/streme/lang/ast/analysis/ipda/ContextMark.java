package streme.lang.ast.analysis.ipda;

import junit.framework.Assert;
import streme.lang.data.IData;
import streme.lang.data.Lst;
import streme.lang.data.Sym;

public class ContextMark implements Mark, IData
{
  
  private IData lambda;
  private IData context;

  public ContextMark(IData lambda, IData context)
  {
    super();
    Assert.assertNotNull(lambda);
    this.lambda = lambda;
    this.context = context;
  }

  public IData getLambda()
  {
    return lambda;
  }
  
  public IData getContext()
  {
    return context;
  }

  public int hashCode()
  {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((context == null) ? 0 : context.hashCode());
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
    final ContextMark other = (ContextMark) obj;
    if (context == null)
    {
      if (other.context != null)
        return false;
    }
    else if (!context.equals(other.context))
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

  public String toString()
  {
    return toData().toString();
  }

  public Lst toData()
  {
    return Lst.valueOf(new Sym("mark"), lambda.toData(), context == null ? new Sym("?context") : context.toData());
  }
}