package streme.lang.eval.nd;

import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.concurrent.Callable;
import java.util.concurrent.atomic.AtomicInteger;

import streme.lang.TCont;
import streme.lang.ast.analysis.RenamingStrategy;
import streme.lang.data.Lst;
import streme.lang.data.Null;
import streme.lang.data.Pair;
import streme.lang.data.Sym;
import streme.lang.eval.MapEnv;

public class QEval
{
  private static final RenamingStrategy RS = new RenamingStrategy()
  {
    AtomicInteger i = new AtomicInteger();

    public Sym rename(Sym original)
    {
      return new Sym("?_" + original + i.incrementAndGet());
    }
  };

  private static Object rename(Object pattern, Map<Sym, Sym> s)
  {
    if (pattern instanceof Sym)
    {
      Sym sym = (Sym) pattern;
      if (sym.getName().startsWith("?"))
      {
        Sym rsym = s.get(sym);
        if (rsym == null)
        {
          rsym = RS.rename(sym);
          s.put(sym, rsym);
        }
        return rsym;
      }
      else
      {
        return pattern;
      }
    }
    if (pattern instanceof Pair)
    {
      Pair p = (Pair) pattern;
      Object rcar = rename(p.car(), s);
      Object rcdr = rename(p.cdr(), s);
      return Pair.cons(rcar, rcdr);
    }
    return pattern;
  }

  public Callable<Callable> qeval(Object query, Map frame, MapEnv env, TSuccess success, TCont fail)
  {
    if (query instanceof Pair)
    {
      Pair pair = (Pair) query;
      Object car = pair.car();
      Lst contents = (Lst) pair.cdr();
      if (car == new Sym("and"))
      {
        return conjoin(contents, frame, env, success, fail);
      }
    }
    return simpleQuery(query, frame, env, success, fail);
  }

  private Callable<Callable> conjoin(Lst contents, Map frame, final MapEnv env, final TSuccess success, TCont fail)
  {
    if (contents.isNull())
    {
      return success.call(frame, fail);
    }
    class ConjoinCont extends TSuccess
    {
      final Lst c;

      public ConjoinCont(Lst c)
      {
        super();
        this.c = c;
      }

      public Callable<Callable> call(Object value, TCont fail)
      {
        Object next = c.cdr();
        return qeval(c.car(), (Map) value, env, next instanceof Null ? success : new ConjoinCont((Lst) next), fail);
      }
    }
    return new ConjoinCont(contents).call(frame, fail);
  }

  private Callable<Callable> simpleQuery(final Object query, final Map frame, final MapEnv env, final TSuccess success,
      final TCont fail)
  {
    class FindAssertionsCont extends TCont
    {
      final Entry[] entries;
      final int i;

      FindAssertionsCont(Entry[] entries, int i)
      {
        super();
        this.entries = entries;
        this.i = i;
      }

      public Callable<Callable> call(Object value)
      {
        int j = i;
        while (j < entries.length)
        {
          Object head = entries[j].getKey();
          Map<Sym, Sym> r = new HashMap<Sym, Sym>();
          Object cleanConclusion = rename(head, r);
          Map extended = new LinkedHashMap(frame);
          if (unifyMatch(query, cleanConclusion, extended))
          {
            Rule rule = (Rule) entries[j].getValue();
            return rule.apply(query, extended, r, env, success, new FindAssertionsCont(entries, j + 1));
          }
          j++;
        }
        return fail.call(null);
      }
    }
    Set<Entry<Object, Object>> entries = env.flatten().entrySet();
    Entry[] flat = entries.toArray(new Entry[entries.size()]);
    return new FindAssertionsCont(flat, 0).call(null);
  }

  public Rule createRule(final Object conclusion, final Object body)
  {
    return new Rule()
    {
      public Callable<Callable> apply(Object pattern, Map frame, Map<Sym, Sym> r, MapEnv env, TSuccess success,
          TCont fail)
      {
        if (body == null)
        {
          return success.call(frame, fail);
        }
        Object cleanBody = rename(body, r);
        return qeval(cleanBody, frame, env, success, fail);
      }
    };
  }

  public Map resolve(Map answer, Object query)
  {
    Map<Sym, Object> map = new LinkedHashMap(answer);
    for (Entry entry : map.entrySet())
    {
      entry.setValue(resolveTerm(entry.getValue(), map));
    }
    Iterator<Entry<Sym, Object>> iter = map.entrySet().iterator();
    Map empty = Collections.emptyMap();
    while (iter.hasNext())
    {
      if (!dependsOn(query, iter.next().getKey(), empty))
      {
        iter.remove();
      }
    }
    return map;
  }

  private Object resolveTerm(Object value, Map map)
  {
    if (isVar(value))
    {
      Object binding = map.get(value);
      if (binding == null)
      {
        return binding;
      }
      return resolveTerm(binding, map);
    }
    if (value instanceof Pair)
    {
      Pair p = (Pair) value;
      return Pair.cons(resolveTerm(p.car(), map), resolveTerm(p.cdr(), map));
    }
    return value;
  }

  // private boolean patternMatch(Object pattern, Object datum, Map frame)
  // {
  // if (pattern.equals(datum))
  // {
  // return true;
  // }
  // if (isVar(pattern))
  // {
  // Object binding = frame.get(pattern);
  // if (binding == null)
  // {
  // frame.put(pattern, datum);
  // return true;
  // }
  // else
  // {
  // return patternMatch(binding, datum, frame);
  // }
  // }
  // if (pattern instanceof Pair && datum instanceof Pair)
  // {
  // Pair l1 = (Pair) pattern;
  // Pair l2 = (Pair) datum;
  // return patternMatch(l1.car(), l2.car(), frame) && patternMatch(l1.cdr(), l2.cdr(), frame);
  // }
  // return false;
  // }
  private boolean unifyMatch(Object pattern, Object datum, Map frame)
  {
    if (pattern.equals(datum))
    {
      return true;
    }
    if (pattern instanceof Sym && ((Sym) pattern).getName().startsWith("?"))
    {
      Object binding = frame.get(pattern);
      if (binding == null)
      {
        frame.put(pattern, datum);
        return true;
      }
      else
      {
        return unifyMatch(binding, datum, frame);
      }
    }
    if (datum instanceof Sym && ((Sym) datum).getName().startsWith("?"))
    {
      Object binding = frame.get(datum);
      if (binding == null)
      {
        frame.put(datum, pattern);
        return true;
      }
      else
      {
        return unifyMatch(binding, pattern, frame);
      }
    }
    if (pattern instanceof Pair && datum instanceof Pair)
    {
      Pair l1 = (Pair) pattern;
      Pair l2 = (Pair) datum;
      return unifyMatch(l1.car(), l2.car(), frame) && unifyMatch(l1.cdr(), l2.cdr(), frame);
    }
    return false;
  }

  private boolean isVar(Object o)
  {
    return o instanceof Sym && ((Sym) o).getName().startsWith("?");
  }

  private boolean extendIfPossible(Sym var, Object val, Map frame)
  {
    Object binding = frame.get(var);
    if (binding == null)
    {
      if (isVar(val))
      {
        Object binding2 = frame.get(val);
        if (binding2 == null)
        {
          frame.put(var, val);
          return true;
        }
        return unifyMatch(var, binding2, frame);
      }
      if (dependsOn(val, var, frame))
      {
        return false;
      }
      frame.put(var, val);
      return true;
    }
    return unifyMatch(binding, val, frame);
  }

  private boolean dependsOn(Object exp, Sym var, Map frame)
  {
    if (isVar(exp))
    {
      if (exp.equals(var))
      {
        return true;
      }
      Object b = frame.get(exp);
      if (b == null)
      {
        return false;
      }
      return dependsOn(b, var, frame);
    }
    if (exp instanceof Pair)
    {
      Pair p = (Pair) exp;
      return dependsOn(p.car(), var, frame) || dependsOn(p.cdr(), var, frame);
    }
    return false;
  }
}
