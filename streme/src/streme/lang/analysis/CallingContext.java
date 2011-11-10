package streme.lang.analysis;

import java.util.Arrays;
import java.util.Set;

public class CallingContext
{
  private Time time;
  private Procedure procedure;
  private Set[] operands;

  public CallingContext(Time time, Procedure procedure, Set[] operands)
  {
    super();
    this.time = time;
    this.procedure = procedure;
    this.operands = operands;
  }

  public int hashCode()
  {
    final int prime = 31;
    int result = 1;
    result = prime * result + Arrays.hashCode(operands);
    result = prime * result + ((procedure == null) ? 0 : procedure.hashCode());
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
    CallingContext other = (CallingContext) obj;
    if (!Arrays.equals(operands, other.operands))
      return false;
    if (procedure == null)
    {
      if (other.procedure != null)
        return false;
    }
    else if (!procedure.equals(other.procedure))
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

  public String toString()
  {
    return "<" + time + " " + procedure + " " + Arrays.toString(operands) + ">";
  }
}
