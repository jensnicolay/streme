package streme.lang.ast.analysis.ipda;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import streme.lang.StremeException;
import streme.lang.ast.Application;
import streme.lang.ast.AstVisitor;
import streme.lang.ast.Binding;
import streme.lang.ast.If;
import streme.lang.ast.Let;
import streme.lang.ast.Node;
import streme.lang.ast.Ref;
import streme.lang.ast.Var;
import streme.lang.ast.Node.Type;
import streme.lang.ast.analysis.RenamingStrategy;
import streme.lang.data.Sym;

public class IpdaDependencyCounter extends AstVisitor
{
  private int counter;
  private Ipda ipda;
  
  public IpdaDependencyCounter(Ipda ipda)
  {
    super();
    this.ipda = ipda;
  }

  public boolean visitLet(Let original)
  {
    if (original.isSimpleLet())
    {
      List<Let> lets = new ArrayList<Let>();
      lets.add(original);
      Node body = original.getBody();
      while (body.type() == Type.LET)
      {
        Let blet = (Let) body;
        if (blet.isSimpleLet())
        {
          lets.add(blet);
          body = blet.getBody();
        }
        else
        {
          break;
        }
      }
      Sym bodyName = RenamingStrategy.NUMBER_RENAMING_STRATEGY.rename(new Sym("body"));
      lets.add(new Let(Let.Kind.LET, new Var(bodyName), body, new Ref(bodyName)));
      if (lets.size() > 2)
      {
        for (int i = 0; i < lets.size(); i++)
        {
          Binding b1 = lets.get(i).getBindings()[0];
          Node n1 = b1.getValue();
          for (int j = i + 1; j < lets.size(); j++)
          {
            Binding b2 = lets.get(j).getBindings()[0];
            Node n2 = b2.getValue();
            if (hasIpdaDependency(ipda, n1, n2))
            {
              counter++;
            }
          }
        }
        return false;
      }
    }
    return true;
  }

  /* only for statistics */
  private boolean hasIpdaDependency(Ipda ipda, Node n1, Node n2)
  {
    Set<Sym> r1 = getIpdaReadDependencies(ipda, n1);
    Set<Sym> r2 = getIpdaReadDependencies(ipda, n2);
    Set<Sym> w1 = getIpdaWriteDependencies(ipda, n1);
    Set<Sym> w2 = getIpdaWriteDependencies(ipda, n2);
    int total = 0;
    total += intersection(r1, w1).size();
    total += intersection(r1, w2).size();
    total += intersection(w1, w2).size();
    return total > 0;
  }

  private Set<Sym> getIpdaReadDependencies(Ipda ipda, Node n)
  {
    switch (n.type())
    {
      case APPLICATION:
        Application application = (Application) n;
        return ipda.monovariantReads(application);
      case IF:
      {
        If ff = (If) n;
        Set<Sym> r = new HashSet<Sym>();
        r.addAll(getIpdaReadDependencies(ipda, ff.getConsequent()));
        r.addAll(getIpdaReadDependencies(ipda, ff.getAlternate()));
        return r;
      }
      case LET:
      {
        Let let = (Let) n;
        Set<Sym> r = new HashSet<Sym>();
        r.addAll(getIpdaReadDependencies(ipda, let.getValue(0)));
        r.addAll(getIpdaReadDependencies(ipda, let.getBody()));
        return r;
      }
      case SETVAR:
      case REF:
      case LITERAL:
      case LAMBDA:
        return new HashSet<Sym>();
      default:
        throw new StremeException("cannot handle " + n);
    }
  }

  private Set<Sym> getIpdaWriteDependencies(Ipda ipda, Node n)
  {
    switch (n.type())
    {
      case APPLICATION:
        Application application = (Application) n;
        return ipda.monovariantWrites(application);
      case IF:
      {
        If ff = (If) n;
        Set<Sym> w = new HashSet<Sym>();
        w.addAll(getIpdaWriteDependencies(ipda, ff.getConsequent()));
        w.addAll(getIpdaWriteDependencies(ipda, ff.getAlternate()));
        return w;
      }
      case LET:
      {
        Let let = (Let) n;
        Set<Sym> w = new HashSet<Sym>();
        w.addAll(getIpdaReadDependencies(ipda, let.getValue(0)));
        w.addAll(getIpdaReadDependencies(ipda, let.getBody()));
        return w;
      }
      case SETVAR:
      case REF:
      case LITERAL:
      case LAMBDA:
        return new HashSet<Sym>();
      default:
        throw new StremeException("cannot handle " + n);
    }
  }

  private <T> Set<T> intersection(Set<T> s1, Set<T> s2)
  {
    Set<T> result = new HashSet<T>(s1);
    result.retainAll(s2);
    return result;
  }

  public int getCounter()
  {
    return counter;
  }
}
