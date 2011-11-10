package streme.lang.ast.analysis.kcfa;

import java.util.ArrayDeque;
import java.util.Collections;
import java.util.Deque;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import streme.lang.StremeException;
import streme.lang.ast.Application;
import streme.lang.ast.AstDataCompiler;
import streme.lang.ast.Lambda;
import streme.lang.ast.Node;
import streme.lang.ast.Ref;
import streme.lang.ast.Var;
import streme.lang.ast.impl.StremeDataCompiler;
import streme.lang.data.Parser2;
import streme.lang.data.Sym;

/**
 * A simple implementation of k-CFA, based on implementation by Matt Might.
 * 
 * @see http://matt.might.net/
 */
public class Kcfa
{
  private class ApplicationState extends State
  {
    private Benv benv;
    private Time time;
    private Application application;

    public ApplicationState(Application application, Benv benv, Store store, Time time)
    {
      super(store);
      this.benv = benv;
      this.application = application;
      this.time = time;
    }

    public int hashCode()
    {
      final int prime = 31;
      int result = super.hashCode();
      result = prime * result + ((application == null) ? 0 : application.hashCode());
      result = prime * result + ((benv == null) ? 0 : benv.hashCode());
      result = prime * result + ((time == null) ? 0 : time.hashCode());
      return result;
    }

    public boolean equals(Object obj)
    {
      if (this == obj)
        return true;
      if (!super.equals(obj))
        return false;
      if (getClass() != obj.getClass())
        return false;
      final ApplicationState other = (ApplicationState) obj;
      if (application == null)
      {
        if (other.application != null)
          return false;
      }
      else if (!application.equals(other.application))
        return false;
      if (benv == null)
      {
        if (other.benv != null)
          return false;
      }
      else if (!benv.equals(other.benv))
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

    public Set<State> next()
    {
      Time time2 = time.tick(application.getTag(), k);
      Node operator = application.getOperator();
      Set<Object> procs = atomEval(operator, benv, getStore());
      Node[] operands = application.getOperands();
      Set[] params = new Set[operands.length];
      for (int i = 0; i < operands.length; i++)
      {
        params[i] = atomEval(operands[i], benv, getStore());
      }
      Set<State> result = new HashSet<State>();
      for (Object proc : procs)
      {
        if (proc instanceof Clo)
        {
          Clo clo = (Clo) proc;
          Lambda lambda = clo.getLambda();
          Benv benv2 = new Benv(clo.getBenv());
          Var[] formals = lambda.getParams();
          Store store2 = new Store(getStore());
          for (int i = 0; i < operands.length; i++)
          {
            Addr addr = alloc(formals[i], time2);
            benv2.extend(formals[i].getName(), addr);
            store2.insert(addr, params[i]);
          }
          Node body = lambda.getBody();
          if (body instanceof Application)
          {
            Application application2 = (Application) body;
            result.add(new ApplicationState(application2, benv2, store2, time2));
          }
          else if (body instanceof Ref)
          {
            result.add(new RefState(benv2, store2, time2));
          }
          else
          {
            throw new StremeException("cannot transition on " + body);
          }
        }
      }
      return result;
    }

    public String toString()
    {
      return "{app " + application.getTag() + " " + benv + " " + getStore() + " " + time + "}";
    }
  }

  private class RefState extends State
  {
    public RefState(Benv benv, Store store, Time time)
    {
      super(store);
    }

    public Set<State> next()
    {
      return Collections.emptySet();
    }
  }

  private int k;

  public Kcfa(int k)
  {
    super();
    this.k = k;
  }

  public Addr alloc(Var var, Time time)
  {
    return new Binding(var.getName(), time);
  }

  public Set<Object> atomEval(Node exp, Benv benv, Store store)
  {
    if (exp instanceof Ref)
    {
      Addr benvLookup = benv.lookup(((Ref) exp).getName());
      if (benvLookup == null)
      {
        throw new IllegalArgumentException("no address for " + exp + " in " + this);
      }
      Set<Object> storeLookup = store.lookup(benvLookup, Object.class);
      return storeLookup;
    }
    if (exp instanceof Lambda)
    {
      Set<Object> d = new HashSet<Object>();
      d.add(new Clo((Lambda) exp, benv));
      return d;
    }
    throw new IllegalArgumentException("unknown expression type " + exp);
  }

  public Set<State> explore(State state)
  {
    Set<State> seen = new HashSet<State>();
    Deque<State> todo = new ArrayDeque<State>();
    todo.push(state);
    while (!todo.isEmpty())
    {
      State t = todo.pop();
      if (seen.contains(t))
      {
        continue;
      }
      seen.add(t);
      todo.addAll(t.next());
    }
    return seen;
  }

  public State makeState(Node exp)
  {
    if (exp instanceof Application)
    {
      return new ApplicationState((Application) exp, new Benv(), new Store(), new Time());
    }
    throw new IllegalArgumentException("cannot create initial state from " + exp);
  }

  public Store summarize(Set<State> states)
  {
    Store store = new Store();
    for (State state : states)
    {
      store.join(state.getStore());
    }
    return store;
  }

  public static void main(String[] args)
  {
    String source = "((lambda (id) (id (lambda (z) z) (lambda (a) (id (lambda (y) y) (lambda (b) b))))) (lambda (x k) (k x)))";
    Parser2 parser = new Parser2();
    Object data = parser.parse(source);
    AstDataCompiler dataCompiler = new StremeDataCompiler();
    Node ast = dataCompiler.compile(data);
    Kcfa cfa = new Kcfa(1);
    State initial = cfa.makeState(ast);
    Set<State> states = cfa.explore(initial);
    Store summary = cfa.summarize(states);
    System.out.println(summary.toStringNice());
    Map<Sym, Set<Object>> monoStore = summary.monovariant();
    System.out.println(monoStore);
  }
}
