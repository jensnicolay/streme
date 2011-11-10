package streme.lang.data;

import java.util.HashMap;
import java.util.Map;
import java.util.TreeMap;

import streme.lang.ast.analysis.RenamingStrategy;
import streme.lang.eval.MacroExpander;

public class DataUnifier
{
  
  public interface ConditionHandler
  {
    Object handle(Sym var, Object value, Object condition);
  }

  private static final RenamingStrategy renamer = RenamingStrategy.NUMBER_RENAMING_STRATEGY;
  private ConditionHandler conditionHandler;
  

  public DataUnifier()
  {
    this(null);
  }

  public DataUnifier(ConditionHandler conditionHandler)
  {
    super();
    this.conditionHandler = conditionHandler;
  }  
  
  public Map<Sym, Object> unify(Object n1, Object n2)
  {
    if (isAny(n1) || isAny(n2))
    {
      return new HashMap<Sym, Object>();
    }
    if (isVar(n2))
    {
      if (occurs(n2, n1))
      {
        return null;
      }
      else
      {
        Map<Sym, Object> s = new HashMap<Sym, Object>();
        s.put((Sym) n2, n1);
        return s;
      }
    }
    if (isVar(n1))
    {
      if (occurs(n1, n2))
      {
        return null;
      }
      else
      {
        Map<Sym, Object> s = new HashMap<Sym, Object>();
        s.put((Sym) n1, n2);
        return s;
      }
    }
    if (n1.equals(n2))
    {
      return new HashMap<Sym, Object>();
    }
    if (n1 instanceof Pair && n2 instanceof Pair)
    {
      Pair p1 = (Pair) n1;
      Pair p2 = (Pair) n2;
      Map<Sym, Object> subs1 = unify(p1.car(), p2.car());
      if (subs1 == null)
      {
        return null;
      }
      Object t1 = p1.cdr();
      Object t2 = p2.cdr();
      t1 = apply(subs1, t1);
      t2 = apply(subs1, t2);
      Map<Sym, Object> subs2 = unify(t1, t2);
      if (subs2 == null)
      {
        return null;
      }
      subs1.putAll(subs2);
      return subs1;
    }
    return null;
  }

  public <T> Object apply(final Map<Sym, T> subs, Object object)
  {
    if (isVar(object))
    {
      Object r = subs.get(object);
      return r == null ? object : r;
    }
    if (object instanceof Pair)
    {
      Pair p = (Pair) object;
      Object car = p.car();
      return Pair.cons(apply(subs, car), apply(subs, p.cdr()));
    }
    return object;
  }

  public Object apply(final Map<Sym, Object> subs, Object object, boolean autoSubstitute)
  {
    if (isVar(object))
    {
      Object r = subs.get(object);
      if (r == null)
      {
        if (autoSubstitute)
        {
          Sym var = (Sym) object;
          r = renamer.rename(var);
          subs.put(var, r);
        }
        else
        {
          r = object;
        }
      }
      return r;
    }
    if (object instanceof Pair)
    {
      Pair p = (Pair) object;
      Object car = p.car();
      if (isVarSplice(car))
      {
        String name = ((Sym) car).getName().substring(1);
        Object r = subs.get(new Sym(name));
        return ((Lst) apply(subs, r, autoSubstitute)).append((Lst) apply(subs, p.cdr(), autoSubstitute));
      }      
      return Pair.cons(apply(subs, car, autoSubstitute), apply(subs, p.cdr(), autoSubstitute));
    }
    return object;
  }

  private boolean occurs(Object n1, Object n2)
  {
    return false;
  }

  private boolean isVar(Object object)
  {
    return object instanceof Sym && ((Sym) object).getName().startsWith("?");
  }
  
  private boolean isVarSplice(Object object)
  {
    return object instanceof Sym && ((Sym) object).getName().startsWith("@?");
  }
  
  private boolean isAny(Object object)
  {
    return object instanceof Sym && ((Sym) object).getName().equals("?");
  }
  
  public boolean matches(Object d1, Object d2)
  {
    return unify(d1, d2) != null;
  }

  public static void main(String[] args)
  {
    DataUnifier u = new DataUnifier();
    Parser2 parser = new Parser2();
    MacroExpander expander = new MacroExpander();
    Object n1 = expander.rewrite(parser.parse("(match ?x root)"));
    System.out.println(n1);
    Object n2 = parser.parse("(match ?pattern ?ast)");
    Map<Sym, Object> result = u.unify(n1, n2);
    System.out.println(result == null ? result : new TreeMap<Sym, Object>(result));
//    Object rewrite = parser.parse("((lock ?lock) @?exps (unlock ?lock))");
//    System.out.println(u.apply(result, rewrite));
  }
}
