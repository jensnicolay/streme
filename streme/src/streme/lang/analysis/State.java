package streme.lang.analysis;

import java.util.Set;

public class State
{
  private Set<Object> values;
  private Time time;

  public State(Set<Object> values, Time time)
  {
    super();
    this.values = values;
    this.time = time;
  }
  
  public int hashCode()
  {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((time == null) ? 0 : time.hashCode());
    result = prime * result + ((values == null) ? 0 : values.hashCode());
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
    State other = (State) obj;
    if (time == null)
    {
      if (other.time != null)
        return false;
    }
    else if (!time.equals(other.time))
      return false;
    if (values == null)
    {
      if (other.values != null)
        return false;
    }
    else if (!values.equals(other.values))
      return false;
    return true;
  }

  public Set<Object> getValues()
  {
    return values;
  }
  
  public Time getTime()
  {
    return time;
  }
  
  public String toString()
  {
    return values + "@" + time;
  }
  
}
