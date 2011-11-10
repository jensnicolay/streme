package streme.lang.eval;

import streme.lang.data.Lst;
import streme.lang.data.Null;
import streme.lang.data.Pair;

public class LstEnv
{

  private LstEnv parent;
  private Lst bindings;

  public LstEnv()
  {
    this(null);
  }
  
  public LstEnv(LstEnv parent)
  {
    super();
    this.parent = parent;
    bindings = new Null();
  }

  public Pair add(Object key, Object value)
  {
    Pair binding = Pair.cons(key, value);
    bindings = Pair.cons(binding, bindings);
    return binding;
  }

  public Pair append(Object key, Object value)
  {
    Pair binding = Pair.cons(key, value);
    bindings = bindings.append(binding);
    return binding;
  }

  public Object get(Object name)
  {
    LstEnv env = this;
    do
    {
      Object value = env.bindings.assoc(name);
      if (!Boolean.FALSE.equals(value))
      {
        return value;
      }
      env = env.parent;
    }
    while (env != null);
    return new Boolean(false);
  }
  
  public LstEnv getParent()
  {
    return parent;
  }
  
  public Lst getBindings()
  {
    return bindings;
  }
  
  public String toString()
  {
    return "<environment@" + Integer.toString(System.identityHashCode(this), 36) + ">";
  }
}
