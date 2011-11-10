package streme.lang.data;

import java.util.Arrays;


public class DataRule
{

  private Object name;
  private Object lhs;
  private Object rhs;
  private Object[] conditions;

  public DataRule(Object name, Object lhs, Object rhs, Object... conditions)
  {
    super();
    this.name = name;
    this.lhs = lhs;
    this.rhs = rhs;
    this.conditions = conditions;
  }

  public Object rhs()
  {
    return rhs;
  }

  public Object lhs()
  {
    return lhs;
  }
  
  public Object[] conditions()
  {
    return conditions;
  }

  public String toString()
  {
    return name + ": " + lhs() + " --> " + rhs() + (conditions.length > 0 ? " if " + Arrays.toString(conditions) : "");
  }

  public static void main(String[] args)
  {
  }
}

