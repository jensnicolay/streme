package streme.lang.ast.analysis.ipda;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;

import streme.lang.ast.Node;
import streme.lang.ast.Var;
import streme.lang.ast.analysis.kcfa.Addr;
import streme.lang.ast.analysis.kcfa.Benv;

public class Kont //implements Comparable<Kont>
{
  
  private static final AtomicInteger TAGGER = new AtomicInteger();
  
  
  public static final Kont halt()
  {
    return new Kont();
  }
  
  private Var var;
  private Node exp;
  private Benv benv;
  private RetPoint retPoint;
  private Set<Mark> marks;
  private int tag = TAGGER.getAndIncrement();

  private Kont()
  {
    this(null, null, null, null);
  }

  public Kont(Var var, Node exp, Benv benv, RetPoint retPoint)
  {
    this(var, exp, benv, retPoint, new HashSet<Mark>());
  }

  private Kont(Var var, Node exp, Benv benv, RetPoint retPoint, Set<Mark> marks)
  {
    super();
    this.var = var;
    this.exp = exp;
    this.benv = benv;
    this.retPoint = retPoint;
    this.marks = marks;
  }
  
  public int getTag()
  {
    return tag;
  }

  public Var getVar()
  {
    return var;
  }

  public Node getExp()
  {
    return exp;
  }

  public Benv getBenv()
  {
    return benv;
  }

  public RetPoint getRetPoint()
  {
    return retPoint;
  }

  public Set<Mark> getMarks()
  {
    return marks;
  }

//  public Kont joinMark(Mark mark)
//  {
//    Set<Mark> newMarks = new HashSet<Mark>(marks);
//    newMarks.add(mark);
//    return new Kont(var, exp, benv, retPoint, newMarks);
//  }
  
  public void mark(Mark mark)
  {
    marks.add(mark);
  }

  public int hashCode()
  {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((benv == null) ? 0 : benv.hashCode());
    result = prime * result + ((exp == null) ? 0 : exp.hashCode());
    result = prime * result + ((retPoint == null) ? 0 : retPoint.hashCode());
    result = prime * result + ((var == null) ? 0 : var.hashCode());
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
    Kont other = (Kont) obj;
    if (benv == null)
    {
      if (other.benv != null)
        return false;
    }
    else if (!benv.equals(other.benv))
      return false;
    if (exp == null)
    {
      if (other.exp != null)
        return false;
    }
    else if (!exp.equals(other.exp))
      return false;
    if (retPoint == null)
    {
      if (other.retPoint != null)
        return false;
    }
    else if (!retPoint.equals(other.retPoint))
      return false;
    if (var == null)
    {
      if (other.var != null)
        return false;
    }
    else if (!var.equals(other.var))
      return false;
    return true;
  }
  
  public Set<Addr> touches()
  {
    if (isHalt())
    {
      return Collections.emptySet();
    }
    Set<Addr> touches = benv.touches();
    touches.add(retPoint);
    return touches;
  }
  
//  public int compareTo(Kont other)
//  {
//    int cvar = var.compareTo(other.var);
//    if (cvar != 0)
//    {
//      return cvar;
//    }
//    int cexp = exp.compareTo(other.exp);
//    if (cexp != 0)
//    {
//      return cexp;
//    }
//    int cbenv = benv.compareTo(other.cbenv);
//    
//  }
  
  public boolean isHalt()
  {
    return exp == null;
  }

  public String toString()
  {
    if (isHalt())
    {
      return "(kont halt)";
    }
    return "(kont-" + getTag() +  " " + var + " " + exp.toShortString() + " " + retPoint + " marks:" + marks.size() + ")";
  }
}
