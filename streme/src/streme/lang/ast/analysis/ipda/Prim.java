package streme.lang.ast.analysis.ipda;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import streme.lang.ast.Node;
import streme.lang.ast.Ref;
import streme.lang.ast.analysis.kcfa.Addr;
import streme.lang.ast.analysis.kcfa.Benv;
import streme.lang.data.IData;
import streme.lang.data.Null;
import streme.lang.data.Sym;

public abstract class Prim implements IData
{
  public static List<Prim> getPrimitives()
  {
    List<Prim> prims = new ArrayList<Prim>();
    prims.add(IIADD);
    prims.add(IIMUL);
    prims.add(IISUB);
    prims.add(FADD);
    prims.add(FMUL);
    prims.add(FSUB);
    prims.add(FDIV);
    prims.add(IADD);
    prims.add(IMUL);
    prims.add(ISUB);
    prims.add(IDIV);
    prims.add(IREMAINDER);
    prims.add(IIREMAINDER);
    prims.add(IMODULO);
    prims.add(IQUOTIENT);
    prims.add(IIQUOTIENT);
    prims.add(EQUAL);
    prims.add(EQUALP);
    prims.add(EQP);
    prims.add(LT);
    prims.add(GT);
    prims.add(GTE);
    prims.add(LTE);
    prims.add(NOT);
    prims.add(IIZEROP);
    prims.add(IZEROP);
    prims.add(IEVENP);
    prims.add(IODDP);
    prims.add(IIEVENP);
    prims.add(IIODDP);
    prims.add(NUMBERP);
    prims.add(NULLP);
    prims.add(CONS);
    prims.add(CAR);
    prims.add(CDR);
    prims.add(SETCAR);
    prims.add(SETCDR);
    prims.add(CAAR);
    prims.add(CDAR);
    prims.add(CADR);
    prims.add(CADDR);
    prims.add(CADDDR);
    prims.add(CDDR);
    prims.add(MEMBER);
    prims.add(MEMV);
    prims.add(LIST);
    prims.add(LENGTH);
    prims.add(LISTREF);
    prims.add(PAIRP);
    prims.add(REVERSE);
    prims.add(APPEND);
    prims.add(ASSQ);
    prims.add(MAP);
    prims.add(FOREACH);
    prims.add(VECTOR);
    prims.add(MAKEVECTOR);
    prims.add(VECTORREF);
    prims.add(VECTORLENGTH);
    prims.add(VECTORSET);
    prims.add(VECTOR2LIST);
    prims.add(NUMBER2STRING);
    prims.add(STRING2SYMBOL);
    prims.add(SYMBOL2STRING);
    prims.add(LIST2VECTOR);
    prims.add(STRINGREF);
    
    prims.add(EXPENSIVEOPERATION);
    prims.add(VERYEXPENSIVEOPERATION);
    prims.add(DISPLAY);
    prims.add(FATALERROR);
    prims.add(FIXEDRANDOMLIST);
    
    return prims;
  }

  private static Set<Object> d(Object... values)
  {
    Set<Object> s = new HashSet<Object>();
    for (Object value : values)
    {
      s.add(value);
    }
    return s;
  }
  
  private static <T> Set<T> setOf(T... values)
  {
    Set<T> s = new HashSet<T>();
    for (T value : values)
    {
      s.add(value);
    }
    return s;
  }  

  private static final Prim EXPENSIVEOPERATION = new Prim(new Sym("expensive-operation"))
  {
    public Set<Object> eval(Set[] args)
    {
      return d(Ipda.ANY);
    }
  };
  
  private static final Prim DISPLAY = new Prim(new Sym("display"))
  {
    public Set<Object> eval(Set[] args)
    {
      return d(Ipda.ANY);
    }
  };  
  
  private static final Prim FATALERROR = new Prim(new Sym("fatal-error"))
  {
    public Set<Object> eval(Set[] args)
    {
      return d(Ipda.ANY);
    }
  };
  
  private static final Prim VERYEXPENSIVEOPERATION = new Prim(new Sym("very-expensive-operation"))
  {
    public Set<Object> eval(Set[] args)
    {
      return d(Ipda.ANY);
    }
  };
  
  private static final Prim FIXEDRANDOMLIST = new Prim(new Sym("fixed-random-list"))
  {
    public Set<Object> eval(Set[] args)
    {
      return d(Ipda.ANY);
    }
  };
  
  private static final Prim CAAR = new Prim(new Sym("caar"))
  {
    public Set<Object> eval(Set[] args)
    {
      return d(Ipda.ANY);
    }
  };
  private static final Prim CADR = new Prim(new Sym("cadr"))
  {
    public Set<Object> eval(Set[] args)
    {
      return d(Ipda.ANY);
    }
  };
  private static final Prim CDAR = new Prim(new Sym("cdar"))
  {
    public Set<Object> eval(Set[] args)
    {
      return d(Ipda.ANY);
    }
  };
  private static final Prim CADDR = new Prim(new Sym("caddr"))
  {
    public Set<Object> eval(Set[] args)
    {
      return d(Ipda.ANY);
    }
  };
  private static final Prim CADDDR = new Prim(new Sym("cadddr"))
  {
    public Set<Object> eval(Set[] args)
    {
      return d(Ipda.ANY);
    }
  };
  private static final Prim CDDR = new Prim(new Sym("cddr"))
  {
    public Set<Object> eval(Set[] args)
    {
      return d(Ipda.ANY);
    }
  };
  private static final Prim MEMBER = new Prim(new Sym("member"))
  {
    public Set<Object> eval(Set[] args)
    {
      return d(Ipda.ANY);
    }
  };
  private static final Prim MEMV = new Prim(new Sym("memv"))
  {
    public Set<Object> eval(Set[] args)
    {
      return d(Ipda.ANY);
    }
  };
  private static final Prim CAR = new Prim(new Sym("car"))
  {
    public Set<Object> eval(Set[] args)
    {
      return d(Ipda.ANY);
    }
  };
  private static final Prim CDR = new Prim(new Sym("cdr"))
  {
    public Set<Object> eval(Set[] args)
    {
      return d(Ipda.ANY);
    }
  };
  private static final Prim VECTORREF = new Prim(new Sym("vector-ref"))
  {
    public Set<Object> eval(Set[] args)
    {
      return d(Ipda.ANY);
    }
  };
  private static final Prim VECTORLENGTH = new Prim(new Sym("vector-length"))
  {
    public Set<Object> eval(Set[] args)
    {
      return d(Ipda.ANY);
    }
  };
  private static final Prim VECTOR = new Prim(new Sym("vector"))
  {
    public Set<Object> eval(Set[] args)
    {
      return d(Ipda.ANY);
    }
  };
  private static final Prim MAKEVECTOR = new Prim(new Sym("make-vector"))
  {
    public Set<Object> eval(Set[] args)
    {
      return d(Ipda.ANY);
    }
  };
  private static final Prim VECTORSET = new Prim(new Sym("vector-set!"))
  {
    public Set<Object> eval(Set[] args)
    {
      return d(Ipda.ANY);
    }
    
    public Set<Addr> writes(Benv benv, Node[] operands)
    {
      return setOf(benv.lookup(((Ref) operands[0]).getName()));
    };
    
  };
  private static final Prim SETCAR = new Prim(new Sym("set-car!"))
  {
    public Set<Object> eval(Set[] args)
    {
      return d(Ipda.ANY);
    }
    
    public Set<Addr> writes(Benv benv, Node[] operands)
    {
      return setOf(benv.lookup(((Ref) operands[0]).getName()));
    };
    
  };
  private static final Prim SETCDR = new Prim(new Sym("set-cdr!"))
  {
    public Set<Object> eval(Set[] args)
    {
      return d(Ipda.ANY);
    }
    
    public Set<Addr> writes(Benv benv, Node[] operands)
    {
      return setOf(benv.lookup(((Ref) operands[0]).getName()));
    };
    
  };
  private static final Prim ASSQ = new Prim(new Sym("assq"))
  {
    public Set<Object> eval(Set[] args)
    {
      return d(Ipda.ANY);
    }
  };
  private static final Prim MAP = new Prim(new Sym("map"))
  {
    public Set<Object> eval(Set[] args)
    {
      return d(Ipda.ANY);
    }
  };
  private static final Prim FOREACH = new Prim(new Sym("for-each"))
  {
    public Set<Object> eval(Set[] args)
    {
      return d(Ipda.ANY);
    }
  };
  private static final Prim CONS = new Prim(new Sym("cons"))
  {
    public Set<Object> eval(Set[] args)
    {
      return d(Ipda.ANY);
    }
  };
  private static final Prim NOT = new Prim(new Sym("not"))
  {
    public Set<Object> eval(Set[] args)
    {
      if (!exact(args))
      {
        return d(Boolean.TRUE, Boolean.FALSE);
      }
      return d(Boolean.FALSE.equals(args[0].iterator().next()));
    }
  };
  private static final Prim LT = new Prim(new Sym("<"))
  {
    public Set<Object> eval(Set[] args)
    {
      if (!exact(args))
      {
        return d(Boolean.TRUE, Boolean.FALSE);
      }
      Comparable r = (Comparable) args[0].iterator().next();
      for (int i = 1; i < args.length; i++)
      {
        Set<Object> arg = args[i];
        if (r.compareTo(arg.iterator().next()) >= 0)
        {
          return d(Boolean.FALSE);
        }
      }
      return d(Boolean.TRUE);
    }
  };
  private static final Prim GT = new Prim(new Sym(">"))
  {
    public Set<Object> eval(Set[] args)
    {
      if (!exact(args))
      {
        return d(Boolean.TRUE, Boolean.FALSE);
      }
      Comparable r = (Comparable) args[0].iterator().next();
      for (int i = 1; i < args.length; i++)
      {
        Set<Object> arg = args[i];
        if (r.compareTo(arg.iterator().next()) <= 0)
        {
          return d(Boolean.FALSE);
        }
      }
      return d(Boolean.TRUE);
    }
  };
  private static final Prim GTE = new Prim(new Sym(">="))
  {
    public Set<Object> eval(Set[] args)
    {
      if (!exact(args))
      {
        return d(Boolean.TRUE, Boolean.FALSE);
      }
      Comparable r = (Comparable) args[0].iterator().next();
      for (int i = 1; i < args.length; i++)
      {
        Set<Object> arg = args[i];
        if (r.compareTo(arg.iterator().next()) < 0)
        {
          return d(Boolean.FALSE);
        }
      }
      return d(Boolean.TRUE);
    }
  };
  private static final Prim LTE = new Prim(new Sym("<="))
  {
    public Set<Object> eval(Set[] args)
    {
      if (!exact(args))
      {
        return d(Boolean.TRUE, Boolean.FALSE);
      }
      Comparable r = (Comparable) args[0].iterator().next();
      for (int i = 1; i < args.length; i++)
      {
        Set<Object> arg = args[i];
        if (r.compareTo(arg.iterator().next()) > 0)
        {
          return d(Boolean.FALSE);
        }
      }
      return d(Boolean.TRUE);
    }
  };
  private static final Prim IIZEROP = new Prim(new Sym("Izero?"))
  {
    public Set<Object> eval(Set[] args)
    {
      if (!exact(args))
      {
        return d(Boolean.TRUE, Boolean.FALSE);
      }
      return d(BigInteger.ZERO.equals(args[0].iterator().next()));
    }
  };
  private static final Prim IZEROP = new Prim(new Sym("zero?"))
  {
    public Set<Object> eval(Set[] args)
    {
      if (!exact(args))
      {
        return d(Boolean.TRUE, Boolean.FALSE);
      }
      return d(Integer.valueOf(0).equals(args[0].iterator().next()));
    }
  };
  private static final Prim IEVENP = new Prim(new Sym("even?"))
  {
    public Set<Object> eval(Set[] args)
    {
      return d(Boolean.TRUE, Boolean.FALSE);
    }
  };
  private static final Prim IODDP = new Prim(new Sym("odd?"))
  {
    public Set<Object> eval(Set[] args)
    {
      return d(Boolean.TRUE, Boolean.FALSE);
    }
  };
  private static final Prim IIEVENP = new Prim(new Sym("Ieven?"))
  {
    public Set<Object> eval(Set[] args)
    {
      return d(Boolean.TRUE, Boolean.FALSE);
    }
  };
  private static final Prim IIODDP = new Prim(new Sym("Iodd?"))
  {
    public Set<Object> eval(Set[] args)
    {
      return d(Boolean.TRUE, Boolean.FALSE);
    }
  };
  private static final Prim EQUAL = new Prim(new Sym("="))
  {
    public Set<Object> eval(Set[] args)
    {
      if (!exact(args))
      {
        return d(Boolean.TRUE, Boolean.FALSE);
      }
      return d(args[0].iterator().next().equals(args[1].iterator().next()));
    }
  };
  private static final Prim EQUALP = new Prim(new Sym("equal?"))
  {
    public Set<Object> eval(Set[] args)
    {
      if (!exact(args))
      {
        return d(Boolean.TRUE, Boolean.FALSE);
      }
      return d(args[0].iterator().next().equals(args[1].iterator().next()));
    }
  };
  private static final Prim EQP = new Prim(new Sym("eq?"))
  {
    public Set<Object> eval(Set[] args)
    {
      return d(Boolean.TRUE, Boolean.FALSE);
    }
  };
  private static final Prim LIST = new Prim(new Sym("list"))
  {
    public Set<Object> eval(Set[] args)
    {
      return d(Ipda.ANY);
    }
  };
  private static final Prim LENGTH = new Prim(new Sym("length"))
  {
    public Set<Object> eval(Set[] args)
    {
      return d(Ipda.ANY);
    }
  };
  private static final Prim LISTREF = new Prim(new Sym("list-ref"))
  {
    public Set<Object> eval(Set[] args)
    {
      return d(Ipda.ANY);
    }
  };
  private static final Prim NULLP = new Prim(new Sym("null?"))
  {
    public Set<Object> eval(Set[] args)
    {
      if (!exact(args))
      {
        return d(Boolean.TRUE, Boolean.FALSE);
      }
      return d(args[0].iterator().next() instanceof Null);
    }
  };
  private static final Prim NUMBERP = new Prim(new Sym("number?"))
  {
    public Set<Object> eval(Set[] args)
    {
      return d(Boolean.TRUE, Boolean.FALSE);
    }
  };
  private static final Prim IISUB = new Prim(new Sym("I-"))
  {
    public Set<Object> eval(Set[] args)
    {
      if (!exact(args))
      {
        return d(Ipda.ANY);
      }
      BigInteger r = (BigInteger) args[0].iterator().next();
      for (int i = 1; i < args.length; i++)
      {
        Set<Object> arg = args[i];
        if (arg.size() != 1)
        {
          return d(Ipda.ANY);
        }
        r = r.subtract((BigInteger) arg.iterator().next());
      }
      return d(r);
    }
  };
  private static final Prim FDIV = new Prim(new Sym("f/"))
  {
    public Set<Object> eval(Set[] args)
    {
      if (!exact(args))
      {
        return d(Ipda.ANY);
      }
      double r = (Double) args[0].iterator().next();
      for (int i = 1; i < args.length; i++)
      {
        Set<Object> arg = args[i];
        if (arg.size() != 1)
        {
          return d(Ipda.ANY);
        }
        r /= (Double) arg.iterator().next();
      }
      return d(r);
    }
  };
  
  private static final Prim FSUB = new Prim(new Sym("f-"))
  {
    public Set<Object> eval(Set[] args)
    {
      if (!exact(args))
      {
        return d(Ipda.ANY);
      }
      double r = (Double) args[0].iterator().next();
      for (int i = 1; i < args.length; i++)
      {
        Set<Object> arg = args[i];
        if (arg.size() != 1)
        {
          return d(Ipda.ANY);
        }
        r -= (Double) arg.iterator().next();
      }
      return d(r);
    }
  };
  
  private static final Prim FMUL = new Prim(new Sym("f*"))
  {
    public Set<Object> eval(Set[] args)
    {
      if (!exact(args))
      {
        return d(Ipda.ANY);
      }
      double r = (Double) args[0].iterator().next();
      for (int i = 1; i < args.length; i++)
      {
        Set<Object> arg = args[i];
        r *= (Double) arg.iterator().next();
      }
      return d(r);
    }
  };
  private static final Prim IIMUL = new Prim(new Sym("I*"))
  {
    public Set<Object> eval(Set[] args)
    {
      if (!exact(args))
      {
        return d(Ipda.ANY);
      }
      BigInteger r = (BigInteger) args[0].iterator().next();
      for (int i = 1; i < args.length; i++)
      {
        Set<Object> arg = args[i];
        r = r.multiply((BigInteger) arg.iterator().next());
      }
      return d(r);
    }
  };
  
  private static final Prim IIADD = new Prim(new Sym("I+"))
  {
    public Set<Object> eval(Set[] args)
    {
      if (!exact(args))
      {
        return d(Ipda.ANY);
      }
      BigInteger r = (BigInteger) args[0].iterator().next();
      for (int i = 1; i < args.length; i++)
      {
        Set<Object> arg = args[i];
        r = r.add((BigInteger) arg.iterator().next());
      }
      return d(r);
    }
  };

  private static final Prim FADD = new Prim(new Sym("f+"))
  {
    public Set<Object> eval(Set[] args)
    {
      if (!exact(args))
      {
        return d(Ipda.ANY);
      }
      double r = (Double) args[0].iterator().next();
      for (int i = 1; i < args.length; i++)
      {
        Set<Object> arg = args[i];
        r += (Double) arg.iterator().next();
      }
      return d(r);
    }
  };

  private static final Prim IADD = new Prim(new Sym("+"))
  {
    public Set<Object> eval(Set[] args)
    {
      if (!exact(args))
      {
        return d(Ipda.ANY);
      }
      int r = (Integer) args[0].iterator().next();
      for (int i = 1; i < args.length; i++)
      {
        Set<Object> arg = args[i];
        r += (Integer) arg.iterator().next();
      }
      return d(r);
    }
  };

  private static final Prim ISUB = new Prim(new Sym("-"))
  {
    public Set<Object> eval(Set[] args)
    {
      if (!exact(args))
      {
        return d(Ipda.ANY);
      }
      int r = (Integer) args[0].iterator().next();
      for (int i = 1; i < args.length; i++)
      {
        Set<Object> arg = args[i];
        r -= (Integer) arg.iterator().next();
      }
      return d(r);
    }
  };

  private static final Prim IMUL = new Prim(new Sym("*"))
  {
    public Set<Object> eval(Set[] args)
    {
      if (!exact(args))
      {
        return d(Ipda.ANY);
      }
      int r = (Integer) args[0].iterator().next();
      for (int i = 1; i < args.length; i++)
      {
        Set<Object> arg = args[i];
        r *= (Integer) arg.iterator().next();
      }
      return d(r);
    }
  };

  private static final Prim IDIV = new Prim(new Sym("/"))
  {
    public Set<Object> eval(Set[] args)
    {
      if (!exact(args))
      {
        return d(Ipda.ANY);
      }
      int r = (Integer) args[0].iterator().next();
      for (int i = 1; i < args.length; i++)
      {
        Set<Object> arg = args[i];
        r /= (Integer) arg.iterator().next();
      }
      return d(r);
    }
  };

  private static final Prim IREMAINDER = new Prim(new Sym("remainder"))
  {
    public Set<Object> eval(Set[] args)
    {
      return d(Ipda.ANY);
    }
  };

  private static final Prim IIREMAINDER = new Prim(new Sym("Iremainder"))
  {
    public Set<Object> eval(Set[] args)
    {
      return d(Ipda.ANY);
    }
  };

  private static final Prim IMODULO = new Prim(new Sym("modulo"))
  {
    public Set<Object> eval(Set[] args)
    {
      return d(Ipda.ANY);
    }
  };
  
  private static final Prim REVERSE = new Prim(new Sym("reverse"))
  {
    public Set<Object> eval(Set[] args)
    {
      return d(Ipda.ANY);
    }
  };

  private static final Prim IQUOTIENT = new Prim(new Sym("quotient"))
  {
    public Set<Object> eval(Set[] args)
    {
      return d(Ipda.ANY);
    }
  };

  private static final Prim IIQUOTIENT = new Prim(new Sym("Iquotient"))
  {
    public Set<Object> eval(Set[] args)
    {
      return d(Ipda.ANY);
    }
  };

  private static final Prim PAIRP = new Prim(new Sym("pair?"))
  {
    public Set<Object> eval(Set[] args)
    {
      return d(Boolean.TRUE, Boolean.FALSE);
    }
  };

  private static final Prim APPEND = new Prim(new Sym("append"))
  {
    public Set<Object> eval(Set[] args)
    {
      return d(Ipda.ANY);
    }
  };

  private static final Prim NUMBER2STRING = new Prim(new Sym("number->string"))
  {
    public Set<Object> eval(Set[] args)
    {
      return d(Ipda.ANY);
    }
  };

  private static final Prim STRING2SYMBOL = new Prim(new Sym("string->symbol"))
  {
    public Set<Object> eval(Set[] args)
    {
      return d(Ipda.ANY);
    }
  };

  private static final Prim SYMBOL2STRING = new Prim(new Sym("symbol->string"))
  {
    public Set<Object> eval(Set[] args)
    {
      return d(Ipda.ANY);
    }
  };

  private static final Prim LIST2VECTOR = new Prim(new Sym("list->vector"))
  {
    public Set<Object> eval(Set[] args)
    {
      return d(Ipda.ANY);
    }
  };

  private static final Prim STRINGREF = new Prim(new Sym("string-ref"))
  {
    public Set<Object> eval(Set[] args)
    {
      return d(Ipda.ANY);
    }
  };

  private static final Prim VECTOR2LIST = new Prim(new Sym("vector->list"))
  {
    public Set<Object> eval(Set[] args)
    {
      return d(Ipda.ANY);
    }
  };

  
  private static boolean exact(Set[] args)
  {
//    for (Set<Object> arg : args)
//    {
//      if (arg.size() != 1 || arg.contains(Ipda.ANY))
//      {
//        return false;
//      }
//    }
//    return true;
    return false;
  }
  
  
  private Sym name;

  public Prim(Sym name)
  {
    super();
    this.name = name;
  }
  
  public int hashCode()
  {
    return name.hashCode();
  }

  public abstract Set<Object> eval(Set[] args);

  public String toString()
  {
    return name.toString();
  }

  public Sym getName()
  {
    return name;
  }

  public Object toData()
  {
    return name;
  }

  public Set<Addr> writes(Benv benv, Node[] operands)
  {
    return Collections.emptySet();
  }
}
