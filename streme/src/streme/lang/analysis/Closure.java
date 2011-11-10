package streme.lang.analysis;

import java.util.Set;

import streme.lang.ast.Lambda;
import streme.lang.ast.Var;

public class Closure extends Procedure
{
  private final StatefulIpdAnalyzer ipdAnalyzer;
  private final Lambda lambda;
  private BindingEnv staticBindingEnv;

  public Closure(StatefulIpdAnalyzer ipdAnalyzer, Lambda lambda, BindingEnv bindingEnv)
  {
    super("#" + lambda.getTag());
    this.ipdAnalyzer = ipdAnalyzer;
    this.lambda = lambda;
    staticBindingEnv = bindingEnv;
  }
  
  public Lambda getLambda()
  {
    return lambda;
  }
  
  public BindingEnv getBindingEnv()
  {
    return staticBindingEnv;
  }

  public void apply(Set<Object>[] operands, final Time dynamicTime, final BindingEnv dynamicBindingEnv, final StackCont dynamicCont)
  {
    Var[] params = lambda.getParams();
    BindingEnv extendedBindingEnv = new BindingEnv(staticBindingEnv);
    for (int i = 0; i < params.length; i++)
    {
      ipdAnalyzer.allocEnv(params[i], dynamicTime, extendedBindingEnv, operands[i]);
    }
    ipdAnalyzer.eval(lambda.getBody(), dynamicTime, extendedBindingEnv, dynamicCont);
  }

  public int hashCode()
  {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((lambda == null) ? 0 : lambda.hashCode());
    result = prime * result + ((staticBindingEnv == null) ? 0 : staticBindingEnv.hashCode());
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
    Closure other = (Closure) obj;
    if (lambda == null)
    {
      if (other.lambda != null)
        return false;
    }
    else if (!lambda.equals(other.lambda))
      return false;
    if (staticBindingEnv == null)
    {
      if (other.staticBindingEnv != null)
        return false;
    }
    else if (!staticBindingEnv.equals(other.staticBindingEnv))
      return false;
    return true;
  }
  
  
}