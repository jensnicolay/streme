package streme.lang.analysis;

import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

import streme.lang.StremeException;
import streme.lang.data.Lst;
import streme.lang.data.Null;
import streme.lang.data.Pair;
import streme.lang.data.Sym;

public class Primitives
{
  interface Op1
  {
    Object calc(Object v1);
  }
  interface Op2
  {
    Object calc(Object v1, Object v2);
  }
  interface Op3
  {
    Object calc(Object v1, Object v2, Object v3);
  }
  private static class P extends Procedure
  {
    public P(String name)
    {
      super(name);
    }

    public void apply(Set<Object>[] operands, Time time, BindingEnv bindingEnv, StackCont cont)
    {
      if (impreciseValues(operands))
      {
        returnAny(time, cont);
        return;
      }
      switch (operands.length)
      {
        case 1:
        {
          cont.call(new State(apply1(operands[0], new Op1()
          {
            public Object calc(Object v1)
            {
              return perform1(v1);
            };
          }), time));
          break;
        }
        case 2:
        {
          cont.call(new State(apply2(operands[0], operands[1], new Op2()
          {
            public Object calc(Object v1, Object v2)
            {
              return perform2(v1, v2);
            };
          }), time));
          break;
        }
        case 3:
        {
          cont.call(new State(apply3(operands[0], operands[1], operands[2], new Op3()
          {
            public Object calc(Object v1, Object v2, Object v3)
            {
              return perform3(v1, v2, v3);
            };
          }), time));
          break;
        }
        default:
        {
          returnAny(time, cont);
        }
      }
    }

    protected Object perform1(Object v1)
    {
      throw new StremeException("not implemented for " + this);
    }

    protected Object perform2(Object v1, Object v2)
    {
      throw new StremeException("not implemented for " + this);
    }

    protected Object perform3(Object v1, Object v2, Object v3)
    {
      throw new StremeException("not implemented for " + this);
    }

    protected Object performN(Object[] vs)
    {
      throw new StremeException("not implemented for " + this);
    }
  }
  private static final Set<Object> SET_OF_TRUE_FALSE = setOf(Boolean.TRUE, Boolean.FALSE);
  private static final Set<Object> SET_OF_TRUE = setOf(Boolean.TRUE);
  private static final Set<Object> SET_OF_FALSE = setOf(Boolean.FALSE);
  private static final Object ANY = new Object()
  {
    public String toString()
    {
      return "any";
    }
  };
  private static final Set<Object> SET_OF_ANY = setOf(ANY);
  private static final int PRECISION = 4;
  private static final Integer INT_ZERO = new Integer(0);
  private static final Map<Sym, Object> PRIMITIVES = new HashMap<Sym, Object>();
  private static final Procedure ZEROP = new Procedure("zero?")
  {
    public void apply(Set<Object>[] operands, Time time, BindingEnv bindingEnv, StackCont cont)
    {
      returnFuzzy(INT_ZERO, operands[0], time, cont);
    }
  };
  private static final Procedure LESSTHAN = new Procedure("<")
  {
    public void apply(Set<Object>[] operands, Time time, BindingEnv bindingEnv, StackCont cont)
    {
      if (impreciseValues(operands))
      {
        returnAny(time, cont);
        return;
      }
      if (operands.length != 2)
      {
        // TODO
        throw new StremeException("illegal number of arguments for <");
      }
      returnFuzzy(Boolean.TRUE, apply2(operands[0], operands[1], new Op2()
      {
        public Object calc(Object v1, Object v2)
        {
          return ((Integer) v1) < ((Integer) v2);
        }
      }), time, cont);
    }
  };
  private static final Procedure PLUS = new P("+")
  {
    protected Object perform1(Object v1)
    {
      return v1;
    }

    protected Object perform2(Object v1, Object v2)
    {
      return streme.lang.eval.Primitives.plus(v1, v2);
    }
  };

  private static final Procedure MULT = new P("*")
  {
    protected Object perform1(Object v1)
    {
      return v1;
    }

    protected Object perform2(Object v1, Object v2)
    {
      return streme.lang.eval.Primitives.multiply(v1, v2);
    }

    protected Object perform3(Object v1, Object v2, Object v3)
    {
      Object t = streme.lang.eval.Primitives.multiply(v1, v2);
      return streme.lang.eval.Primitives.multiply(t, v3);
    }
  };

  private static final Procedure MIN = new P("-")
  {
    protected Object perform1(Object v1)
    {
      return streme.lang.eval.Primitives.minus(v1);
    }

    protected Object perform2(Object v1, Object v2)
    {
      return streme.lang.eval.Primitives.minus(v1, v2);
    }
  };
  private static final Procedure DIV = new P("/")
  {
    protected Object perform1(Object v1)
    {
      return streme.lang.eval.Primitives.divide(v1);
    }

    protected Object perform2(Object v1, Object v2)
    {
      return streme.lang.eval.Primitives.divide(v1, v2);
    }
  };
  private static final Procedure CONS = new P("cons")
  {
    protected Object perform2(Object v1, Object v2)
    {
      return Pair.cons(v1, v2);
    }
  };
  private static final Procedure CAR = new P("car")
  {
    protected Object perform1(Object v1)
    {
      return ((Lst) v1).car();
    }
  };
  private static final Procedure CDR = new P("cdr")
  {
    protected Object perform1(Object v1)
    {
      return ((Lst) v1).cdr();
    }
  };
  private static final Procedure APPEND = new P("append")
  {
    protected Object perform2(Object v1, Object v2)
    {
      return ((Lst) v1).append((Lst) v2);
    };
  };

  private static final Procedure AV = new Procedure("av")
  {
    public void apply(Set<Object>[] operands, Time time, BindingEnv bindingEnv, StackCont cont)
    {
      Set<Object> result = new HashSet<Object>();
      for (Set<Object> operand : operands)
      {
        result.add(operand.iterator().next());
      }
      cont.call(new State(result, time));
    }
  };

  private static final Procedure LIST = new Procedure("list")
  {
    public void apply(Set<Object>[] operands, Time time, BindingEnv bindingEnv, StackCont cont)
    {
      if (impreciseValues(operands))
      {
        returnAny(time, cont);
        return;
      }
      Set<Object> result = new HashSet<Object>();
      int n = operands.length;
      int[] m = new int[n];
      Object[][] operands2 = new Object[n][];
      for (int i = 0; i < n; i++)
      {
        m[i] = operands[i].size();
        operands2[i] = operands[i].toArray();
      }
      for (int[] v : new PG(m))
      {
        Lst l = new Null();
        for (int i = n - 1; i >= 0; i--)
        {
          l = Pair.cons(operands2[i][v[i]], l);
        }
        result.add(l);
      }
      cont.call(new State(result, time));
    }
  };

  static
  {
    PRIMITIVES.put(new Sym("<"), LESSTHAN);
    PRIMITIVES.put(new Sym("/"), DIV);
    PRIMITIVES.put(new Sym("-"), MIN);
    PRIMITIVES.put(new Sym("*"), MULT);
    PRIMITIVES.put(new Sym("+"), PLUS);
    PRIMITIVES.put(new Sym("append"), APPEND);
    PRIMITIVES.put(new Sym("av"), AV);
    PRIMITIVES.put(new Sym("car"), CAR);
    PRIMITIVES.put(new Sym("cdr"), CDR);
    PRIMITIVES.put(new Sym("cons"), CONS);
    PRIMITIVES.put(new Sym("list"), LIST);
    PRIMITIVES.put(new Sym("zero?"), ZEROP);
  }

  private static Set<Object> apply1(Set<Object> operands1, Op1 op)
  {
    Set<Object> result = new HashSet<Object>();
    for (Object operand1 : operands1)
    {
      result.add(op.calc(operand1));
    }
    return result;
  }

  private static Set<Object> apply2(Set<Object> operands1, Set<Object> operands2, Op2 op)
  {
    Set<Object> result = new HashSet<Object>();
    for (Object operand1 : operands1)
    {
      for (Object operand2 : operands2)
      {
        result.add(op.calc(operand1, operand2));
      }
    }
    return result;
  }

  private static Set<Object> apply3(Set<Object> operands1, Set<Object> operands2, Set<Object> operands3, Op3 op)
  {
    Set<Object> result = new HashSet<Object>();
    for (Object operand1 : operands1)
    {
      for (Object operand2 : operands2)
      {
        for (Object operand3 : operands3)
        {
          result.add(op.calc(operand1, operand2, operand3));
        }
      }
    }
    return result;
  }

  public static Object get(Sym name)
  {
    return PRIMITIVES.get(name);
  }

  public static boolean impreciseValue(Set<Object> value)
  {
    if (value.size() > PRECISION)
    {
      return true;
    }
    if (value.contains(ANY))
    {
      return true;
    }
    return false;
  }

  private static boolean impreciseValues(Set<Object>[] values)
  {
    for (Set<Object> value : values)
    {
      if (impreciseValue(value))
      {
        return true;
      }
    }
    return false;
  }

  private static void returnAny(Time time, StackCont cont)
  {
    cont.call(new State(SET_OF_ANY, time));
  }

  private static void returnFalse(Time time, StackCont cont)
  {
    cont.call(new State(SET_OF_FALSE, time));
  }

  public static void returnFuzzy(Object trueValue, Set<Object> values, Time time, StackCont cont)
  {
    if (impreciseValue(values))
    {
      cont.call(new State(SET_OF_TRUE_FALSE, time));
    }
    else if (values.size() == 1 && values.contains(trueValue))
    {
      cont.call(new State(SET_OF_TRUE, time));
    }
    else if (values.contains(trueValue))
    {
      cont.call(new State(SET_OF_TRUE_FALSE, time));
    }
    else
    {
      returnFalse(time, cont);
    }
  }

  private static void returnValue(Object value, Time time, StackCont cont)
  {
    cont.call(new State(setOf(value), time));
  }

  private static Set<Object> setOf(Object... els)
  {
    Set<Object> set = new LinkedHashSet<Object>();
    for (Object el : els)
    {
      set.add(el);
    }
    return set;
  }
}
