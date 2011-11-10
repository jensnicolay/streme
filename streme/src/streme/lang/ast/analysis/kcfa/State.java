package streme.lang.ast.analysis.kcfa;

import java.util.Set;


public abstract class State
{
  
  private Store store;

  public State(Store store)
  {
    super();
    this.store = store;
  }  
  
  public int hashCode()
  {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((store == null) ? 0 : store.hashCode());
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
    final State other = (State) obj;
    if (store == null)
    {
      if (other.store != null)
        return false;
    }
    else if (!store.equals(other.store))
      return false;
    return true;
  }

  public Store getStore()
  {
    return store;
  }

  public abstract Set<State> next();
}