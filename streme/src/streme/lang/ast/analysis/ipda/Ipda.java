package streme.lang.ast.analysis.ipda;

import java.io.BufferedReader;
import java.io.FileReader;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Deque;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.logging.Level;
import java.util.logging.Logger;

import junit.framework.Assert;

import org.jgrapht.DirectedGraph;
import org.jgrapht.Graphs;
import org.jgrapht.graph.DefaultEdge;
import org.jgrapht.graph.SimpleDirectedGraph;
import org.jgrapht.traverse.BreadthFirstIterator;

import streme.lang.Logging;
import streme.lang.StremeException;
import streme.lang.ast.Application;
import streme.lang.ast.AstAnalyzer;
import streme.lang.ast.AstDataCompiler;
import streme.lang.ast.If;
import streme.lang.ast.Lambda;
import streme.lang.ast.Let;
import streme.lang.ast.Literal;
import streme.lang.ast.Node;
import streme.lang.ast.Node.Type;
import streme.lang.ast.Ref;
import streme.lang.ast.SetVar;
import streme.lang.ast.Var;
import streme.lang.ast.analysis.AnfConverter;
import streme.lang.ast.analysis.RenamingStrategy;
import streme.lang.ast.analysis.Undefiner;
import streme.lang.ast.analysis.kcfa.Addr;
import streme.lang.ast.analysis.kcfa.Benv;
import streme.lang.ast.analysis.kcfa.Binding;
import streme.lang.ast.analysis.kcfa.Clo;
import streme.lang.ast.analysis.kcfa.Store;
import streme.lang.ast.analysis.kcfa.Time;
import streme.lang.ast.impl.AlphaConverter;
import streme.lang.ast.impl.Printer;
import streme.lang.ast.impl.StremeDataCompiler;
import streme.lang.ast.impl.TagPrinter;
import streme.lang.data.DataUnifier;
import streme.lang.data.IData;
import streme.lang.data.Parser2;
import streme.lang.data.Sym;
import streme.lang.eval.MacroExpander;

public class Ipda implements AstAnalyzer
{
  public enum Dependency
  {
    RW, WW, WR, RR
  };

  public static final Logger STATE_LOGGER = Logger.getLogger("state");
  public static final Logger ANALYZE_LOGGER = Logger.getLogger("analyze");
  
  public static final Object ANY = "any";

  private abstract class State implements IState
  {
    private int number = stateCounter.incrementAndGet();

    public int getNumber()
    {
      return number;
    }
  }

  private static class ApplyFunContext
  {
    private Clo clo;
    private RetPoint retPoint;
    private Time time;

    public ApplyFunContext(Clo clo, RetPoint retPoint, Time time)
    {
      super();
      this.clo = clo;
      this.retPoint = retPoint;
      this.time = time;
    }

    public int hashCode()
    {
      final int prime = 7;
      int result = 1;
      result = prime * result + ((clo == null) ? 0 : clo.hashCode());
      result = prime * result + ((retPoint == null) ? 0 : retPoint.hashCode());
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
      final ApplyFunContext other = (ApplyFunContext) obj;
      if (clo == null)
      {
        if (other.clo != null)
          return false;
      }
      else if (!clo.equals(other.clo))
        return false;
      if (retPoint == null)
      {
        if (other.retPoint != null)
          return false;
      }
      else if (!retPoint.equals(other.retPoint))
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
  }

  private class ApplyFunState extends State
  {
    private Clo clo;
    private RetPoint retPoint;
    private Store store;
    private Time time;
    private Set[] values;

    public ApplyFunState(Clo clo, Set[] values, Store store, RetPoint retPoint, Time time)
    {
      super();
      this.clo = clo;
      this.values = values;
      this.store = store;
      this.retPoint = retPoint;
      this.time = time;
    }

    public boolean equals(Object obj)
    {
      if (this == obj)
        return true;
      if (obj == null)
        return false;
      if (getClass() != obj.getClass())
        return false;
      ApplyFunState other = (ApplyFunState) obj;
      if (clo == null)
      {
        if (other.clo != null)
          return false;
      }
      else if (!clo.equals(other.clo))
        return false;
      if (retPoint == null)
      {
        if (other.retPoint != null)
          return false;
      }
      else if (!retPoint.equals(other.retPoint))
        return false;
      if (time == null)
      {
        if (other.time != null)
          return false;
      }
      else if (!time.equals(other.time))
        return false;
      if (!Arrays.equals(values, other.values))
        return false;
      if (store == null)
      {
        if (other.store != null)
          return false;
      }
      else if (!store.equals(other.store))
      {
        return false;
      }
      return true;
    }

    public void join(IState state)
    {
      Assert.assertEquals(getContext(), state.getContext());
      ApplyFunState other = (ApplyFunState) state;
      store.join(other.store);
      for (int i = 0; i < values.length; i++)
      {
        Set<Object> vs = new HashSet<Object>(values[i]);
        vs.addAll(other.values[i]);
        values[i] = vs;
      }
    }

    public Store getStore()
    {
      return store;
    }

    public int hashCode()
    {
      final int prime = 31;
      int result = 1;
      result = prime * result + ((clo == null) ? 0 : clo.hashCode());
      result = prime * result + ((retPoint == null) ? 0 : retPoint.hashCode());
      result = prime * result + ((store == null) ? 0 : store.hashCode());
      result = prime * result + ((time == null) ? 0 : time.hashCode());
      result = prime * result + Arrays.hashCode(values);
      return result;
    }

    public Object getContext()
    {
      return new ApplyFunContext(clo, retPoint, time);
    }

    public Set<Kont> konts()
    {
      return store.lookup(retPoint, Kont.class);
    }

    public List<IState> next()
    {
      Lambda lambda = clo.getLambda();
      Var[] params = lambda.getParams();
      Node body = lambda.getBody();
      Benv benv = clo.getBenv();
      Benv benv2 = new Benv(benv);
      Store store2 = new Store(store);
      for (int i = 0; i < params.length; i++)
      {
        Var param = params[i];
        Sym u = param.getName();
        allocHeap(u, time, values[i], benv2, store2);
      }
      List<IState> result = new ArrayList<IState>();
      result.add(new EvalTailState(body, benv2, store2, retPoint, time));
      return result;
    }

    public Set<Addr> reads()
    {
      return Collections.emptySet();
    }

    public String toString()
    {
      return "{applyFun " + clo + " " + Arrays.toString(values) + "}";
    }

    public Set<Addr> writes()
    {
      return Collections.emptySet();
    }

    public IState gc()
    {
      return this;
    }
  }

  private static class ApplyKontContext
  {
    private Kont kont;
    private Time time;

    public ApplyKontContext(Kont kont, Time time)
    {
      super();
      this.kont = kont;
      this.time = time;
    }

    public int hashCode()
    {
      final int prime = 11;
      int result = 1;
      result = prime * result + ((kont == null) ? 0 : kont.hashCode());
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
      final ApplyKontContext other = (ApplyKontContext) obj;
      if (kont == null)
      {
        if (other.kont != null)
          return false;
      }
      else if (!kont.equals(other.kont))
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
  }

  private class ApplyKontState extends State
  {
    private Kont kont;
    private Store store;
    private Time time;
    private Set<Object> value;

    public ApplyKontState(Kont kont, Set<Object> value, Store store, Time time)
    {
      super();
      this.kont = kont;
      this.value = value;
      this.store = store;
      this.time = time;
    }

    public boolean equals(Object obj)
    {
      if (this == obj)
        return true;
      if (obj == null)
        return false;
      if (getClass() != obj.getClass())
        return false;
      ApplyKontState other = (ApplyKontState) obj;
      if (kont == null)
      {
        if (other.kont != null)
          return false;
      }
      else if (!kont.equals(other.kont))
        return false;
      if (time == null)
      {
        if (other.time != null)
          return false;
      }
      else if (!time.equals(other.time))
        return false;
      if (value == null)
      {
        if (other.value != null)
          return false;
      }
      else if (!value.equals(other.value))
        return false;
      if (store == null)
      {
        if (other.store != null)
          return false;
      }
      else if (!store.equals(other.store))
      {
        return false;
      }
      return true;
    }

    public void join(IState state)
    {
      Assert.assertEquals(getContext(), state.getContext());
      ApplyKontState other = (ApplyKontState) state;
      store.join(other.store);
      Set<Object> v = new HashSet<Object>(value);
      v.addAll(other.value);
      value = v;
    }

    public Store getStore()
    {
      return store;
    }

    public int hashCode()
    {
      final int prime = 31;
      int result = 1;
      result = prime * result + ((kont == null) ? 0 : kont.hashCode());
      result = prime * result + ((store == null) ? 0 : store.hashCode());
      result = prime * result + ((time == null) ? 0 : time.hashCode());
      result = prime * result + ((value == null) ? 0 : value.hashCode());
      return result;
    }

    public Object getContext()
    {
      return new ApplyKontContext(kont, time);
    }

    public Set<Kont> konts()
    {
      return setOf(kont);
    }

    public List<IState> next()
    {
      List<IState> result = new ArrayList<IState>();
      if (kont.isHalt())
      {
        answer.addAll(value);
        return result;
      }
      Node exp = kont.getExp();
      Var var = kont.getVar();
      Benv benv = kont.getBenv();
      RetPoint rp = kont.getRetPoint();
      if (var != null)
      {
        Sym u = var.getName();
        Benv benv2 = new Benv(benv);
        Store store2 = new Store(store);
        allocHeap(u, time, value, benv2, store2);
        result.add(new EvalTailState(exp, benv2, store2, rp, time));
      }
      else
      {
        result.add(new EvalTailState(exp, benv, store, rp, time));        
      }
      return result;
    }

    public Set<Addr> reads()
    {
      return Collections.emptySet();
    }

    public String toString()
    {
      return "{applyKont " + kont + " " + value + "}";
    }

    public Set<Addr> writes()
    {
      return Collections.emptySet();
    }

    public IState gc()
    {
      return this;
    }
  }

  private static class EvalHeadContext
  {
    private Benv benv;
    private Node exp;
    private Kont kont;
    private Time time;

    public EvalHeadContext(Benv benv, Node exp, Kont kont, Time time)
    {
      super();
      this.benv = benv;
      this.exp = exp;
      this.kont = kont;
      this.time = time;
    }

    public int hashCode()
    {
      final int prime = 13;
      int result = 1;
      result = prime * result + ((benv == null) ? 0 : benv.hashCode());
      result = prime * result + ((exp == null) ? 0 : exp.hashCode());
      result = prime * result + ((kont == null) ? 0 : kont.hashCode());
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
      final EvalHeadContext other = (EvalHeadContext) obj;
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
      if (kont == null)
      {
        if (other.kont != null)
          return false;
      }
      else if (!kont.equals(other.kont))
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
  }

  private class EvalHeadState extends State
  {
    private Benv benv;
    private Node exp;
    private Kont kont;
    private Store store;
    private Time time;

    public EvalHeadState(Node exp, Benv benv, Store store, Kont kont, Time time)
    {
      super();
      Assert.assertNotNull(exp);
      this.exp = exp;
      this.benv = benv;
      this.store = store;
      this.kont = kont;
      this.time = time;
    }

    public boolean equals(Object obj)
    {
      if (this == obj)
        return true;
      if (obj == null)
        return false;
      if (getClass() != obj.getClass())
        return false;
      EvalHeadState other = (EvalHeadState) obj;
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
      if (kont == null)
      {
        if (other.kont != null)
          return false;
      }
      else if (!kont.equals(other.kont))
        return false;
      if (time == null)
      {
        if (other.time != null)
          return false;
      }
      else if (!time.equals(other.time))
        return false;
      if (store == null)
      {
        if (other.store != null)
          return false;
      }
      else if (!store.equals(other.store))
      {
        return false;
      }
      return true;
    }

    public void join(IState state)
    {
      Assert.assertEquals(getContext(), state.getContext());
      EvalHeadState other = (EvalHeadState) state;
      store.join(other.store);
    }

    public Store getStore()
    {
      return store;
    }

    public int hashCode()
    {
      final int prime = 31;
      int result = 1;
      result = prime * result + ((benv == null) ? 0 : benv.hashCode());
      result = prime * result + ((exp == null) ? 0 : exp.hashCode());
      result = prime * result + ((kont == null) ? 0 : kont.hashCode());
      result = prime * result + ((store == null) ? 0 : store.hashCode());
      result = prime * result + ((time == null) ? 0 : time.hashCode());
      return result;
    }

    public Object getContext()
    {
      return new EvalHeadContext(benv, exp, kont, time);
    }

    public Set<Kont> konts()
    {
      return setOf(kont);
    }

    public List<IState> next()
    {
      switch (exp.type())
      {
        case APPLICATION:
        {
          Application application = (Application) exp;
          Time time2 = time.tick(application.getTag(), k);
          Set<Object> procs = atomEval(application.getOperator(), benv, store);
          Node[] operands = application.getOperands();
          Set[] vs = new Set[operands.length];
          for (int i = 0; i < operands.length; i++)
          {
            vs[i] = atomEval(operands[i], benv, store);
          }
          List<IState> result = new ArrayList<IState>();
          for (Object proc : procs)
          {
            if (proc instanceof Clo)
            {
              Store store2 = new Store(store);
              Clo clo = (Clo) proc;
              mark(kont, clo.getLambda(), application);
              RetPoint rp = allocStack(clo.getLambda(), time, kont, store2);
              result.add(new ApplyFunState(clo, vs, store2, rp, time2));
            }
            else if (proc instanceof Prim)
            {
              Prim prim = (Prim) proc;
              mark(kont, prim, application);
              Set<Object> value = prim.eval(vs);
              STATE_LOGGER.finer(prim + " " + Arrays.toString(vs) + " ==> " + value);
              result.add(new ApplyKontState(kont, value, store, time2));
            }
            else
            {
              throw new StremeException("cannot handle procedure " + proc);
            }
          }
          return result;
        }
        default:
          throw new StremeException("evalHead cannot handle " + exp);
      }
    }

    public Set<Addr> reads()
    {
      return stateReads(benv, exp);
    }

    public String toString()
    {
      return "{evalHead " + exp.toShortString() + "}";
    }

    public Set<Addr> writes()
    {
      return stateWrites(benv, store, exp);
    }

    public IState gc()
    {
      // Set<Addr> touches = benv.touches();
      // // touches.addAll(kont.touches());
      // store.narrow(reachable(store, touches));
      return this;
    }
  }

  private static class EvalTailContext
  {
    private Benv benv;
    private Node exp;
    private RetPoint retPoint;
    private Time time;

    public EvalTailContext(Benv benv, Node exp, RetPoint retPoint, Time time)
    {
      super();
      this.benv = benv;
      this.exp = exp;
      this.retPoint = retPoint;
      this.time = time;
    }

    public int hashCode()
    {
      final int prime = 17;
      int result = 1;
      result = prime * result + ((benv == null) ? 0 : benv.hashCode());
      result = prime * result + ((exp == null) ? 0 : exp.hashCode());
      result = prime * result + ((retPoint == null) ? 0 : retPoint.hashCode());
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
      final EvalTailContext other = (EvalTailContext) obj;
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
      if (time == null)
      {
        if (other.time != null)
          return false;
      }
      else if (!time.equals(other.time))
        return false;
      return true;
    }
  }

  private class EvalTailState extends State
  {
    private Benv benv;
    private Node exp;
    private RetPoint retPoint;
    private Store store;
    private Time time;

    public EvalTailState(Node exp, Benv benv, Store store, RetPoint retPoint, Time time)
    {
      super();
      Assert.assertNotNull(exp);
      this.exp = exp;
      this.benv = benv;
      this.store = store;
      this.retPoint = retPoint;
      this.time = time;
      //this.seq = seq;
    }

    private List<IState> bindingMutation()
    {
      SetVar setVar = (SetVar) exp;
      Var var = setVar.getVar();
      Addr addr = benv.lookup(var.getName());
      Set<Object> value = atomEval(setVar.getValue(), benv, store);
      List<IState> result = new ArrayList<IState>();
      Store store2 = new Store(store);
      store2.update(addr, value);
      result.add(new EvalTailState(Literal.UNDEFINED, benv, store2, retPoint, time));
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
      EvalTailState other = (EvalTailState) obj;
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
      if (time == null)
      {
        if (other.time != null)
          return false;
      }
      else if (!time.equals(other.time))
        return false;
      if (store == null)
      {
        if (other.store != null)
          return false;
      }
      else if (!store.equals(other.store))
      {
        return false;
      }
      return true;
    }

    public void join(IState state)
    {
      Assert.assertEquals(getContext(), state.getContext());
      EvalTailState other = (EvalTailState) state;
      store.join(other.store);
    }

    public Object getContext()
    {
      return new EvalTailContext(benv, exp, retPoint, time);
    }

    public Store getStore()
    {
      return store;
    }

    public int hashCode()
    {
      final int prime = 31;
      int result = 1;
      result = prime * result + ((benv == null) ? 0 : benv.hashCode());
      result = prime * result + ((exp == null) ? 0 : exp.hashCode());
      result = prime * result + ((retPoint == null) ? 0 : retPoint.hashCode());
      result = prime * result + ((store == null) ? 0 : store.hashCode());
      result = prime * result + ((time == null) ? 0 : time.hashCode());
      return result;
    }

    public Set<Kont> konts()
    {
      return store.lookup(retPoint, Kont.class);
    }

    private List<IState> letBinding()
    {
      Let let = (Let) exp;
      if (let.getBindings().length != 1)
      {
        throw new StremeException("cannot handle " + let);
      };
      Var letName = let.getName(0);
      Node letValue = let.getValue(0);
      Node letBody = let.getBody();
      switch (let.getKind())
      {
        case LET:
//          if (letValue.type() == Type.APPLICATION)
//          {
//            Application application = (Application) letValue;
//            // Time time2 = time.tick(application.getTag(), k);
//            Kont kont = new Kont(letName, letBody, benv, retPoint);
//            List<IState> result = new ArrayList<IState>();
//            result.add(new EvalHeadState(application, benv, store, kont, time));
//            return result;
//          }
//          else if (letValue.type() == Type.SETVAR)
//          {
//            SetVar setVar = (SetVar) letValue;
//            IpdaCoverageChecker.setCovered(setVar);
//            Var var = setVar.getVar();
//            IpdaCoverageChecker.setCovered(var);            
//            Addr addr = benv.lookup(var.getName());
//            Set<Object> value = atomEval(setVar.getValue(), benv, store);
//            List<IState> result = new ArrayList<IState>();
//            Benv benv2 = new Benv(benv);
//            Store store2 = new Store(store);
//            store2.update(addr, value);
//            allocHeap(letName.getName(), time, d(new Quoted(null)), benv2, store2);
//            result.add(new EvalTailState(letBody, benv2, store2, retPoint, time));
//            return result;          }
//          else
//          {
//            List<IState> result = new ArrayList<IState>();
//            Set<Object> v = atomEval(letValue, benv, store);
//            Benv benv2 = new Benv(benv);
//            Store store2 = new Store(store);
//            allocHeap(letName.getName(), time, v, benv2, store2);
//            result.add(new EvalTailState(letBody, benv2, store2, retPoint, time));
//            return result;
//          }
          switch (letValue.type())
          {
            case APPLICATION:
            {
              Application application = (Application) letValue;
              // Time time2 = time.tick(application.getTag(), k);
              Kont kont = new Kont(letName, letBody, benv, retPoint);
              List<IState> result = new ArrayList<IState>();
              result.add(new EvalHeadState(application, benv, store, kont, time));
              return result;              
            }
            case LITERAL:
            case REF:
            case LAMBDA:
            {
              List<IState> result = new ArrayList<IState>();
              Set<Object> v = atomEval(letValue, benv, store);
              Benv benv2 = new Benv(benv);
              Store store2 = new Store(store);
              allocHeap(letName.getName(), time, v, benv2, store2);
              result.add(new EvalTailState(letBody, benv2, store2, retPoint, time));
              return result;
            }
//            case SETVAR:
//            {
//              SetVar setVar = (SetVar) letValue;
//              IpdaCoverageChecker.setCovered(setVar);
//              Var var = setVar.getVar();
//              IpdaCoverageChecker.setCovered(var);            
//              Addr addr = benv.lookup(var.getName());
//              Set<Object> value = atomEval(setVar.getValue(), benv, store);
//              List<IState> result = new ArrayList<IState>();
//              Benv benv2 = new Benv(benv);
//              Store store2 = new Store(store);
//              store2.update(addr, value);
//              allocHeap(letName.getName(), time, d(new Quoted(null)), benv2, store2);
//              result.add(new EvalTailState(letBody, benv2, store2, retPoint, time));
//              return result;   
//            }
            case SETVAR:
            case IF:
            {
              Kont kont = new Kont(letName, letBody, benv, retPoint);
              Store store2 = new Store(store);
              RetPoint rp2 = allocStack(let, time, kont, store2);
              List<IState> result = new ArrayList<IState>();
              result.add(new EvalTailState(letValue, benv, store2, rp2, time));
              return result;
            }
            default:
              throw new StremeException("cannot handle binding expression " + letValue);
          }
        case LETREC:
          Lambda lambda = (Lambda) letValue;
          Sym u = letName.getName();
          Benv benv2 = new Benv(benv);
          Set<Object> clo = atomEval(lambda, benv2, store);
          Store store2 = new Store(store);
          allocHeap(u, time, clo, benv2, store2);
          List<IState> result = new ArrayList<IState>();
          result.add(new EvalTailState(letBody, benv2, store2, retPoint, time));
          return result;
        default:
          throw new StremeException("cannot handle " + let);
      }
    }

    public List<IState> next()
    {
      switch (exp.type())
      {
        case LAMBDA:
        case REF:
        case LITERAL:
          return retrn();
        case APPLICATION:
          return tailCall();
        case SETVAR:
          return bindingMutation();
        case LET:
          return letBinding();
        case IF:
          return conditional();
        default:
          throw new StremeException("cannot handle " + exp);
      }
    }

    public Set<Addr> reads()
    {
      return stateReads(benv, exp);
    }


    private List<IState> retrn()
    {
      List<IState> result = new ArrayList<IState>();
      Set<Object> v = atomEval(exp, benv, store);
      Set<Kont> konts = store.lookup(retPoint, Kont.class);
      for (Kont k : konts)
      {
        result.add(new ApplyKontState(k, v, store, time));
      }
      return result;
    }

    private List<IState> tailCall()
    {
      Application application = (Application) exp;
      List<IState> result = new ArrayList<IState>();
      Time time2 = time.tick(application.getTag(), k);
      Node operator = application.getOperator();
      Set<Object> procs = atomEval(operator, benv, store);
      Node[] operands = application.getOperands();
      Set[] vs = new Set[operands.length];
      for (int i = 0; i < operands.length; i++)
      {
        vs[i] = atomEval(operands[i], benv, store);
      }
      for (Object proc : procs)
      {
        if (proc instanceof Clo)
        {
          Clo clo = (Clo) proc;
          Lambda lambda = clo.getLambda();
          Set<Kont> konts = store.lookup(retPoint, Kont.class);
          for (Kont kont : konts)
          {
            mark(kont, lambda, application);
          }
          result.add(new ApplyFunState(clo, vs, store, retPoint, time2));
        }
        else if (proc instanceof Prim)
        {
          Prim prim = (Prim) proc;
          Set<Object> value = prim.eval(vs);
          STATE_LOGGER.finer(prim + " " + Arrays.toString(vs) + " ==> " + value);
          Set<Kont> konts = store.lookup(retPoint, Kont.class);
          for (Kont kont : konts)
          {
            mark(kont, prim, application);
            result.add(new ApplyKontState(kont, value, store, time2));
          }
        }
        else
        {
          throw new StremeException("cannot handle " + proc);
        }
      }
      return result;
    }

    private List<IState> conditional()
    {
      If iff = (If) exp;
      Set<Object> values = atomEval(iff.getCondition(), benv, store);
      List<IState> result = new ArrayList<IState>();
      if (values.equals(d(Boolean.FALSE)))
      {
        result.add(new EvalTailState(iff.getAlternate(), benv, store, retPoint, time));
      }
      else if (!values.isEmpty() && !values.contains(ANY) && !values.contains(Boolean.FALSE))
      {
        result.add(new EvalTailState(iff.getConsequent(), benv, store, retPoint, time));
      }
      else
      {
        result.add(new EvalTailState(iff.getAlternate(), benv, store, retPoint, time));
        result.add(new EvalTailState(iff.getConsequent(), benv, store, retPoint, time));
      }
      return result;
    }

    public String toString()
    {
      return "{evalTail " + exp.toShortString() + "}";
    }

    public Set<Addr> writes()
    {
      return stateWrites(benv, store, exp);
    }

    public IState gc()
    {
      Set<Addr> touches = benv.touches();
      touches.add(retPoint);
      STATE_LOGGER.fine(exp.toShortString() + " roots " + touches);
      Store store2 = new Store(store);
      store2.narrow(reachable(store, touches));
      return new EvalTailState(exp, benv, store2, retPoint, time);
    }
  }

  private Map<Sym, Prim> primitives;
  private DataUnifier unifier;
  /*
   * INPUT
   */
  /** Binding precision. * */
  private int k;
  /*
   * OUTPUT
   */
  private Map<Sym, Set<Object>> store;
  private DirectedGraph<Object, DefaultEdge> dependencyGraph;
  private Set<Object> answer;
  private boolean useGarbageCollection;
  private AtomicInteger stateCounter;
  private int states;
  private int steps;

  public Ipda(int k)
  {
    super();
    this.k = k;
    dependencyGraph = new SimpleDirectedGraph<Object, DefaultEdge>(DefaultEdge.class);
    store = new HashMap<Sym, Set<Object>>();
    unifier = new DataUnifier();
    primitives = new HashMap<Sym, Prim>();
    for (Prim prim : Prim.getPrimitives())
    {
      primitives.put(prim.getName(), prim);
    }
    useGarbageCollection = true;
  }

  public void setUseGarbageCollection(boolean useGarbageCollection)
  {
    this.useGarbageCollection = useGarbageCollection;
  }

  private Prim primLookup(Sym name)
  {
    return primitives.get(name);
  }

  private void allocHeap(Sym name, Time context, Set<Object> value, Benv benv, Store store)
  {
    Binding binding = new Binding(name, context);
    benv.extend(name, binding);
    store.insert(binding, value);
  }

  private RetPoint allocStack(Node node, Time time, Kont kont, Store store)
  {
    RetPoint rp = new RetPoint(node, time);
    // Set<Object> l = store.safeLookup(rp);
    // if (l != null && l.size() > 1)
    // {
    // System.out.println(l);
    // }
    //    
    store.insert(rp, d(kont));
    return rp;
  }

  private Set<Mark> aggregateMarks(IState state)
  {
    Store store = state.getStore();
    Set<Mark> marks = new HashSet<Mark>();
    Set<Kont> konts = walkStack(store, state.konts());
    for (Kont kont : konts)
    {
      marks.addAll(kont.getMarks());
    }
    return marks;
  }

  /**
   * @return number of states
   */
  public Object analyze(Node node)
  {
    answer = d();
    stateCounter = new AtomicInteger();
    steps = 0;
    IState initial = stateInjector(node);
    Set<IState> states = explore(initial);
    for (IState state : states)
    {
      Set<Mark> marks = aggregateMarks(state);
      ANALYZE_LOGGER.finer("marks for " + state + " = " + marks);
      Set<Addr> reads = state.reads();
      ANALYZE_LOGGER.fine(state + " reads " + reads);
      Set<Addr> writes = state.writes();
      ANALYZE_LOGGER.fine(state + " writes " + writes);
      // System.out.println("==================");
      // System.out.println(state);
      // System.out.println("r " + state.reads());
      // System.out.println("w " + state.writes());
      // System.out.println("s " + state.getStore());
      // System.out.println("ms " + marks);
      // System.out.println("==================\n");
      for (Mark mark : marks)
      {
        dependencyGraph.addVertex(mark);
        for (Addr r : reads)
        {
          dependencyGraph.addVertex(r);
          dependencyGraph.addEdge(r, mark);
          ANALYZE_LOGGER.finer(r + " ---> " + mark);
        }
        for (Addr w : writes)
        {
          dependencyGraph.addVertex(w);
          dependencyGraph.addEdge(mark, w);
          ANALYZE_LOGGER.finer(mark + " ---> " + mark);
        }
      }
    }
    Store summary = summarize(states);
    store.putAll(summary.monovariant());
    ANALYZE_LOGGER.fine("*************");
    //ANALYZE_LOGGER.fine(GraphUtils.dumpGraph(dependencyGraph));
    String dotName = System.getProperty("ipdaDotName");
    if (dotName != null)
    {
      dumpDot(dotName + "-ipda", dependencyGraph);
    }
    ANALYZE_LOGGER.fine("*************");
    return null;
  }

  private Set<Object> atomEval(Node exp, Benv benv, Store store)
  {
    switch (exp.type())
    {
      case REF:
        Ref ref = (Ref) exp;
        Sym name = ref.getName();
        Addr benvLookup = benv.lookup(name);
        if (benvLookup == null)
        {
          Prim prim = primLookup(name);
          if (prim == null)
          {
            STATE_LOGGER.warning("lookup for " + exp + " failed");
            return d();
          }
          else
          {
            return d(prim);
          }
        }
        return store.lookup(benvLookup, Object.class);
      case LAMBDA:
        return d(new Clo((Lambda) exp, benv));
      case LITERAL:
      {
        Literal literal = (Literal) exp;
        Object value = literal.getValue();
        return d(value);
      }
      default:
        throw new StremeException("cannot handle " + exp);
    }
  }
  
  private Set<Object> d(Object... values)
  {
    Set<Object> s = new HashSet<Object>();
    for (Object value : values)
    {
      s.add(value);
    }
    return s;
  }

  private Set<IState> explore(IState state)
  {
    Map<Object, IState> seen = new HashMap<Object, IState>();
    Deque<IState> todo = new ArrayDeque<IState>();
    todo.push(state);
    todo: while (!todo.isEmpty())
    {
      STATE_LOGGER.finer("todo: " + todo.size());
      steps++;
      IState t = todo.pop();
      STATE_LOGGER.finer("=========");
      STATE_LOGGER.fine("popped " + t + " (#" + t.getNumber() + ")");
      if (useGarbageCollection)
      {
        t = t.gc();
      }
      Object context = t.getContext();
      IState st = seen.get(context);
      if (st != null)
      {
        if (st.equals(t))
        {
          STATE_LOGGER.fine("state " + t + " is equal to context state " + st);
          STATE_LOGGER.finer("^^^^^^^^^");
          continue todo;
        }
        st.join(t);
      }
      else
      {
        seen.put(context, t);
        st = t;
      }
      List<IState> next = st.next();
      for (IState n : next)
      {
        STATE_LOGGER.finer("\t--> " + n.getNumber() + "\t" + n);
      }
      todo.addAll(next);
      STATE_LOGGER.finer("^^^^^^^^^");
    }
    states = seen.size();
    return new HashSet<IState>(seen.values());
  }

  private IState stateInjector(Node exp)
  {
    Store store = new Store();
    Time time = new Time();
    RetPoint rp = new RetPoint(null, time);
    store.insert(rp, d(Kont.halt()));
    return new EvalTailState(exp, new Benv(), store, rp, time);
  }

  private void mark(Kont kont, IData lambda, IData context)
  {
    Mark mark = new ContextMark(lambda, context);
    STATE_LOGGER.fine(mark + " for " + kont);
    kont.mark(mark);
  }

  private Set<Addr> stateReads(Benv benv, Node... nodes)
  {
    Set<Addr> result = new HashSet<Addr>();
    for (Node node : nodes)
    {
      switch (node.type())
      {
        case REF:
          Ref ref = (Ref) node;
          Addr lookup = benv.lookup(ref.getName());
          if (lookup == null)
          {
            if (primLookup(ref.getName()) == null)
            {
              STATE_LOGGER.warning("lookup failed for " + node);
            }
          }
          else
          {
            result.add(lookup);
          }
          break;
        case APPLICATION:
          Application application = (Application) node;
          Node operator = application.getOperator();
          Node[] operands = application.getOperands();
          result.addAll(stateReads(benv, operator));
          result.addAll(stateReads(benv, operands));
          break;
        case SETVAR:
          SetVar setVar = (SetVar) node;
          result.addAll(stateReads(benv, setVar.getValue()));
          break;
        case LET:
          Let let = (Let) node;
          result.addAll(stateReads(benv, let.getValue(0)));
          break;
        case IF:
          If iff = (If) node;
          result.addAll(stateReads(benv, iff.getCondition()));
          break;
        case LAMBDA:
        case LITERAL:
          // nothing
          break;
        default:
          throw new StremeException("cannot handle " + node);
      }
    }
    return result;
  }

  private <T> Set<T> setOf(T... values)
  {
    Set<T> s = new HashSet<T>();
    for (T value : values)
    {
      s.add(value);
    }
    return s;
  }

  private Store summarize(Set<IState> states)
  {
    Store store = new Store();
    for (IState state : states)
    {
      store.join(state.getStore());
    }
    return store;
  }

  private Set<Kont> walkStack(Store store, Set<Kont> konts)
  {
    Set<Kont> seen = new HashSet<Kont>();
    Deque<Kont> todo = new ArrayDeque<Kont>(konts);
    while (!todo.isEmpty())
    {
      Kont kont = todo.pop();
      if (seen.contains(kont))
      {
        continue;
      }
      seen.add(kont);
      if (kont.isHalt())
      {
        continue;
      }
      todo.addAll(store.lookup(kont.getRetPoint(), Kont.class));
    }
    return seen;
  }

  private Set<Addr> reachable(Store store, Set<Addr> roots)
  {
    Set<Addr> reachable = new HashSet<Addr>();
    Deque<Addr> todo = new ArrayDeque<Addr>(roots);
    while (!todo.isEmpty())
    {
      Addr addr = todo.pop();
      if (reachable.contains(addr))
      {
        continue;
      }
      reachable.add(addr);
      Set<Object> values = store.lookup(addr, Object.class);
      STATE_LOGGER.finest("checking " + addr + "->" + values);
      for (Object value : values)
      {
        Set<Addr> touches;
        if (value instanceof Clo)
        {
          touches = ((Clo) value).touches();
        }
        else if (value instanceof Kont)
        {
          touches = ((Kont) value).touches();
        }
        else
        {
          touches = Collections.emptySet();
        }
        todo.addAll(touches);
        if (STATE_LOGGER.isLoggable(Level.FINER) && !touches.isEmpty())
        {
          STATE_LOGGER.finer(addr + " touches " + touches + " via " + value);
        }
      }
    }
    return reachable;
  }

  public String toString()
  {
    Iterator<Object> iter = vertexIterator();
    StringBuilder sb = new StringBuilder();
    while (iter.hasNext())
    {
      Object v = iter.next();
      if (v instanceof Mark)
      {
        Mark mark = (Mark) v;
        Set<Object> neighbours = new HashSet<Object>(Graphs.neighborListOf(dependencyGraph, mark));
        for (Object neighbour : neighbours)
        {
          if (dependencyGraph.containsEdge(neighbour, mark))
          {
            sb.append(mark + " reads " + neighbour + "\n");
          }
          if (dependencyGraph.containsEdge(mark, neighbour))
          {
            sb.append(mark + " writes " + neighbour + "\n");
          }
        }
      }
    }
    sb.append("states: " + states + ", steps: " + steps + "\n");
    sb.append("answer: " + answer + "\n");
    sb.append("store: " + store + "\n");
    return sb.toString();
  }

  private BreadthFirstIterator<Object, DefaultEdge> vertexIterator()
  {
    return new BreadthFirstIterator<Object, DefaultEdge>(dependencyGraph);
  }

  public Set<Object> getAnswer()
  {
    return answer;
  }

  public boolean writes(Mark procedure, Addr resource)
  {
    checkVertex(procedure);
    checkVertex(resource);
    boolean procToResource = dependencyGraph.containsEdge(procedure, resource);
    return procToResource;
  }

  public boolean reads(Mark procedure, Addr resource)
  {
    checkVertex(procedure);
    checkVertex(resource);
    boolean resourceToProc = dependencyGraph.containsEdge(resource, procedure);
    return resourceToProc;
  }

  private void checkVertex(Object vertex)
  {
    if (!dependencyGraph.containsVertex(vertex))
    {
      throw new StremeException("unknown vertex " + vertex);
    }
  }

  public Set<Dependency> getDependencies(Mark proc1, Mark proc2)
  {
    checkVertex(proc1);
    checkVertex(proc2);
    Set<Addr> reads1 = reads(proc1);
    Set<Addr> reads2 = reads(proc2);
    Set<Addr> writes1 = writes(proc1);
    Set<Addr> writes2 = writes(proc2);
    Set<Dependency> dependencies = new HashSet<Dependency>(4);
    Set<Addr> writeReadDeps = new HashSet<Addr>(writes1);
    writeReadDeps.retainAll(reads2);
    if (!writeReadDeps.isEmpty())
    {
      ANALYZE_LOGGER.fine("W/R deps " + writeReadDeps + " for " + proc1 + "/" + proc2);
      dependencies.add(Dependency.WR);
    }
    Set<Addr> writeWriteDeps = new HashSet<Addr>(writes1);
    writeWriteDeps.retainAll(writes2);
    if (!writeWriteDeps.isEmpty())
    {
      ANALYZE_LOGGER.fine("W/W deps " + writeWriteDeps + " for " + proc1 + "/" + proc2);
      dependencies.add(Dependency.WW);
    }
    Set<Addr> readWriteDeps = new HashSet<Addr>(reads1);
    readWriteDeps.retainAll(writes2);
    if (!readWriteDeps.isEmpty())
    {
      ANALYZE_LOGGER.fine("R/W deps " + readWriteDeps + " for " + proc1 + "/" + proc2);
      dependencies.add(Dependency.RW);
    }
    Set<Addr> readReadDeps = new HashSet<Addr>(reads1);
    readReadDeps.retainAll(reads2);
    if (!readReadDeps.isEmpty())
    {
      ANALYZE_LOGGER.fine("R/R deps " + readReadDeps + " for " + proc1 + "/" + proc2);
      dependencies.add(Dependency.RR);
    }
    return dependencies;
  }

  public Set<Addr> writes(Mark mark)
  {
    Set<DefaultEdge> outgoings = dependencyGraph.outgoingEdgesOf(mark);
    Set<Addr> writes = new HashSet<Addr>();
    for (DefaultEdge outgoing : outgoings)
    {
      writes.add((Addr) dependencyGraph.getEdgeTarget(outgoing));
    }
    return writes;
  }

  public Set<Addr> reads(Mark mark)
  {
    Set<DefaultEdge> incomings = null;
    try
    {
      incomings = dependencyGraph.incomingEdgesOf(mark);
    }
    catch (IllegalArgumentException iae)
    {
      throw new StremeException(String.valueOf(mark), iae);
    }
    Set<Addr> reads = new HashSet<Addr>();
    for (DefaultEdge incoming : incomings)
    {
      reads.add((Addr) dependencyGraph.getEdgeSource(incoming));
    }
    return reads;
  }
  
  public Set<Sym> monovariantReads(Application application)
  {
    Set<Mark> marks = findMarks(application);
    Set<Sym> result = new HashSet<Sym>();
    for (Mark m : marks)
    {
      for (Addr a : reads(m))
      {
        if (a instanceof Binding)
        {
          result.add(((Binding) a).getVar());
        }
      }
    }
    return result;
  }
  
  public Set<Sym> monovariantWrites(Application application)
  {
    Set<Mark> marks = findMarks(application);
    Set<Sym> result = new HashSet<Sym>();
    for (Mark m : marks)
    {
      for (Addr a : writes(m))
      {
        if (a instanceof Binding)
        {
          result.add(((Binding) a).getVar());
        }
      }
    }
    return result;
  }
  

  public Set<Mark> findMarks(Application application)
  {
    Set<Mark> result = new HashSet<Mark>();
    Node rator = application.getOperator();
    if (rator.type() == Type.REF)
    {
      Set<Object> ls = lookupSym(((Ref) rator).getName());
      if (ls == null)
      {
        throw new StremeException("no lambdas for application " + application);
      }
      for (Object l : ls)
      {
        if (l instanceof Lambda || l instanceof Prim)
        {
          result.add(new ContextMark((IData) l, application));
        }
      }
    }
    return result;
  }

  public Set<Dependency> getDependencies(Application n1, Application n2)
  {
    Set<Dependency> dependencies = new HashSet<Dependency>();
    Set<Mark> m1s = findMarks(n1);
    Set<Mark> m2s = findMarks(n2);
    for (Mark m1 : m1s)
    {
      for (Mark m2 : m2s)
      {
        dependencies.addAll(getDependencies(m1, m2));
      }
    }
    return dependencies;
  }

  private Set<Object> lookupSym(Sym name)
  {
    Set<Object> lookup = store.get(name);
    if (lookup != null)
    {
      return lookup;
    }
    Prim primLookup = primLookup(name);
    if (primLookup == null)
    {
      return null;
    }
    return d(primLookup);
  }

  public Set<Object> flowsTo(Sym sym)
  {
    return store.get(sym);
  }

  public <T extends IData> List<T> findVertices(Object data, Class<T> c)
  {
    BreadthFirstIterator<Object, DefaultEdge> iter = vertexIterator();
    List<T> result = new ArrayList<T>();
    while (iter.hasNext())
    {
      IData vertex = (IData) iter.next();
      if (unifier.matches(vertex.toData(), data))
      {
        result.add((T) vertex);
      }
    }
    return result;
  }

  public IData findVertex(Object data)
  {
    List<IData> result = findVertices(data, IData.class);
    switch (result.size())
    {
      case 0:
        return null;
      case 1:
        return result.get(0);
      default:
        throw new StremeException("more than one match for " + data + ": " + result);
    }
  }

  private Set<Addr> stateWrites(Benv benv, Store store, Node exp)
  {
    if (exp.type() == Type.SETVAR)
    {
      SetVar setVar = (SetVar) exp;
      Set<Addr> result = new HashSet<Addr>();
      result.add(benv.lookup(setVar.getVar().getName()));
      return result;
    }
    else if (exp.type() == Type.LET)
    {
      Let let = (Let) exp;
      return stateWrites(benv, store, let.getValue(0));
    }
    else if (exp.type() == Type.APPLICATION)
    {
      Application application = (Application) exp;
      Node operator = application.getOperator();
      Set<Object> value = atomEval(operator, benv, store);
      Set<Addr> result = new HashSet<Addr>();
      for (Object v : value)
      {
        if (v instanceof Prim)
        {
          Prim prim = (Prim) v;
          result.addAll(prim.writes(benv, application.getOperands()));
        }
      }
      return result;
    }
    return Collections.emptySet();
  }
  
  public static void dumpDot(String name, DirectedGraph<Object, DefaultEdge> g)
  {
//    try
//    {
//      Writer sw = new StringWriter();
//      PrintWriter pw = new PrintWriter(sw, true); 
//      pw.println("digraph G {");
//      for (Object v : g.vertexSet())
//      {
//        if (v instanceof Addr)
//        {
//          pw.println(nodeString(v) + " [shape=box]");
//        }
//        for (DefaultEdge e : g.outgoingEdgesOf(v))
//        {
//          pw.println(nodeString(v) + " -> " + nodeString(g.getEdgeTarget(e)));
//        }
//      }
//      pw.println("}");
//      pw.close();
//      sw.close();
//      
//      Process process = Runtime.getRuntime().exec("dot -Tps2");
//      PrintWriter writer = new PrintWriter(process.getOutputStream(), true);
//      writer.println(sw.toString());
//      process.getOutputStream().write(26);
//      process.getOutputStream().flush();
//      
//      BufferedReader reader = new BufferedReader(new InputStreamReader(process.getInputStream()));
//      PrintWriter fw = new PrintWriter(name + ".eps");
//      String line;
//      while ((line = reader.readLine()) != null)
//      {
//        fw.println(line);
//      }
//      fw.close();
//      
//      BufferedReader error = new BufferedReader(new InputStreamReader(process.getErrorStream()));
//      while ((line = error.readLine()) != null)
//      {
//        System.err.println(line);
//      }
//      
//      Writer dotWriter = new FileWriter(name + ".dot");
//      dotWriter.write(sw.toString());
//      dotWriter.close();
//
//    }
//    catch (Exception e)
//    {
//      e.printStackTrace();
//    }
  }
  
  private static String nodeString(Object v)
  {
    if (v instanceof ContextMark)
    {
      ContextMark mark = (ContextMark) v;
      IData proc = mark.getLambda();
      if (proc instanceof Node)
      {
        return "\"(lam" + ((Node) proc).getTag() + ", " + ((Application) mark.getContext()) + ")\"";
      }
      else
      {
        return "\"(" + proc + ", " + ((Node) mark.getContext()).toShortString() + ")\"";
      }
    }
    else if (v instanceof Binding)
    {
      Binding binding = (Binding) v;
      return "\"(" + binding.getVar() + ", " + binding.getTime() + ")\"";      
    }
    return "\"" + v + "\"";
  }    

  /** for testing purposes */
  private static String load(String name)
  {
    String fileName = "src/test/" + name;
    try
    {
      BufferedReader reader = new BufferedReader(new FileReader(fileName));
      StringBuilder b = new StringBuilder();
      String s;
      while ((s = reader.readLine()) != null)
      {
        b.append(s).append("\n");
      }
      reader.close();
      return b.toString();
    }
    catch (Exception e)
    {
      throw new StremeException("cannot load " + fileName, e);
    }
  }

  public static void main(String[] args)
  {
    Logging.setup(Level.ALL);
    STATE_LOGGER.setLevel(Level.FINE);
    ANALYZE_LOGGER.setLevel(Level.FINER);
    Store.LOGGER.setLevel(Level.FINE);
    Time.LOGGER.setLevel(Level.FINE);
     //String source = "(let ((id (lambda (x q) (q x)))) (id 3 (lambda (v1) (id 4 (lambda (v2) (halt v2))))))";
    //String source ="(let ((f (lambda (n) 0))) (let ((g (f 3))) g))";
    //String source = "(let ((z 0)) (let ((writez (lambda () (set! z 123)))) (let ((readz (lambda () z))) (let ((result (cons (writez) (readz)))) result))))"; 
  //String source = "(letrec ((f (lambda (n) (if (zero? n) 0 (f (- n 1)))))) (f 100))";
    //String source = "(letrec ((fact (lambda (n) (if (zero? n) 1 (* n (fact (- n 1))))))) (fact 3))";
    //String source = "(letrec ((fib (lambda (n)  (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))))) (fib 2))"; System.setProperty("ipdaDotName", "fib-ipda");
    //String source = load("gabriel/tak.str");
     //String source = load("gabriel/boyer.str");
    //String source = load("gabriel/browse.str");
   //String source = load("gabriel/triangl.str");
   // String source = load("gabriel/nboyer.str");
    //String source = load("other/mbrotcore.str");
    //String source = load("other/nqueens.str");
    //String source = "(let ((z 0)) (let ((writez (lambda () (set! z 123)))) (let ((readz (lambda () z))) (begin (readz) (writez)))))";
    //String source = "(let* ((z '()) (appender (lambda (f a b) (append (f a) (f b)))) (conser (lambda (x) (set! z (cons x z)) z))) (appender conser 1 2))";
    String source = "(let* ((z '()) (appender (lambda (f a b) (append (f a) (f b)))) (square (lambda (x) (list (* x x))))) (appender square 1 2))";
    //String source="(let ((l '())) (letrec ((f (lambda (i) (if (zero? i) i (begin (set! l (cons i l)) (f (- i 1))))))) (f 10) (f 10) l)))";
    //String source = "(let ((z 0)) (let ((writez (lambda () (set! z 123)))) (let ((readz (lambda () z))) (let ((p1 (writez))) (let ((p2 (readz))) (cons p1 p2))))))"; System.setProperty("ipdaDotName", "ex1-ipda");
    //String source = "(let ((c (cons 'a 'b))) (let ((writec (lambda () (set-car! c 123)))) (writec)))";
    
	Parser2 parser = new Parser2();
    Object data = parser.parse(source);
    MacroExpander expander = new MacroExpander();
    expander.setLetToLambda(false);
    expander.setLetrecToLambda(false);
    Object expanded = expander.rewrite(data);
        
    AstDataCompiler dataCompiler = new StremeDataCompiler();
    Node ast = Undefiner.undefine(dataCompiler.compile(expanded));
    System.out.println("ast: " + Printer.print(ast));    
//    System.out.println("eval ast: " + new Streme().evalAst(ast));
    
    AlphaConverter alphaConverter = new AlphaConverter(RenamingStrategy.NUMBER_RENAMING_STRATEGY);
    Node alphaAst = alphaConverter.rewrite(ast);    
    System.out.println("alp: " + Printer.print(alphaAst));    
//    System.out.println("eval alpha: " + new Streme().evalAst(alphaAst));
    
    AnfConverter anfConverter = new AnfConverter(RenamingStrategy.NUMBER_RENAMING_STRATEGY);
    Node anf = anfConverter.rewrite(alphaAst);
    System.out.println("anf: " + Printer.print(anf));    
//    System.out.println("eval anf: " + new Streme().evalAst(anf));
    
    TagPrinter tp = new TagPrinter();
    anf.accept(tp);
    System.out.println(tp.toString());
    long start = System.currentTimeMillis();
    Ipda ipda = new Ipda(3);
    ipda.setUseGarbageCollection(true);
    ipda.analyze(anf);
    long end = System.currentTimeMillis();
    
    // JGraph jgraph = new JGraph( new JGraphModelAdapter( ipda.dependencyGraph ));
    // JFrame frame = new JFrame("HelloWorldSwing");
    // frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    // frame.getContentPane().add(jgraph);
    // frame.pack();
    // frame.setVisible(true);
    System.out.println("anf: " + anf);
    System.out.println("answer: " + ipda.getAnswer());
    System.out.println(ipda);
    System.out.println((end - start) + "ms");
  }
}
