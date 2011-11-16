package streme.lang.analysis;

import java.util.ArrayDeque;
import java.util.Collections;
import java.util.Deque;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

import streme.lang.StremeException;
import streme.lang.ast.Application;
import streme.lang.ast.AstAnalyzer;
import streme.lang.ast.Begin;
import streme.lang.ast.Binding;
import streme.lang.ast.Define;
import streme.lang.ast.If;
import streme.lang.ast.Lambda;
import streme.lang.ast.Let;
import streme.lang.ast.Literal;
import streme.lang.ast.Node;
import streme.lang.ast.Ref;
import streme.lang.ast.SetVar;
import streme.lang.ast.Var;
import streme.lang.ast.impl.StremeDataCompiler;
import streme.lang.ast.impl.TagPrinter;
import streme.lang.data.Parser2;
import streme.lang.eval.nd.NdAnalysisStreme;

/**
 * Stateful IPD analyzer.
 */
public class StatefulIpdAnalyzer implements AstAnalyzer<Void>
{
  private final class BeginStackCont extends StackCont
  {
    private final StackCont cont;
    private final Node[] exps;
    private final int index;

    private BeginStackCont(BindingEnv bindingEnv, Node[] exps, int index, StackCont cont)
    {
      super("begin", cont, bindingEnv);
      this.cont = cont;
      this.exps = exps;
      this.index = index;
    }

    public void call(State state)
    {
      if (index == exps.length - 2)
      {
        eval(exps[index + 1], state.getTime(), getBindingEnv(), cont);
      }
      else
      {
        eval(exps[index + 1], state.getTime(), getBindingEnv(), new BeginStackCont(getBindingEnv(), exps, index + 1, cont));
      }
    }
  }

  private final class LetStackCont extends StackCont
  {
    private boolean letstar;
    private final StackCont cont;
    private final Binding[] bindings;
    private final Node body;
    private final int index;

    private LetStackCont(boolean letstar, BindingEnv bindingEnv, Binding[] bindings, final Node body, int index,
        StackCont cont)
    {
      super("begin", cont, bindingEnv);
      this.letstar = letstar;
      this.cont = cont;
      this.bindings = bindings;
      this.body = body;
      this.index = index;
    }

    public void call(State state)
    {
      allocEnv(bindings[index].getVar(), state.getTime(), getBindingEnv(), state.getValues());
      if (index == bindings.length - 1)
      {
        eval(body, state.getTime(), getBindingEnv(), cont);
      }
      else
      {
        BindingEnv newBindingEnv = letstar ? new BindingEnv(getBindingEnv()) : getBindingEnv();
        eval(bindings[index + 1].getValue(), state.getTime(), newBindingEnv, new LetStackCont(letstar, newBindingEnv,
            bindings, body, index + 1, cont));
      }
    }
  }

  private final class LetRecStackCont extends StackCont
  {
    private final StackCont cont;
    private final Binding[] bindings;
    private final Node body;
    private final int index;

    private LetRecStackCont(BindingEnv bindingEnv, Binding[] bindings, final Node body, int index,
        StackCont cont)
    {
      super("begin", cont, bindingEnv);
      this.cont = cont;
      this.bindings = bindings;
      this.body = body;
      this.index = index;
    }

    public void call(State state)
    {
      updateEnv(bindings[index].getVar(), getBindingEnv(), state.getValues());
      if (index == bindings.length - 1)
      {
        eval(body, state.getTime(), getBindingEnv(), cont);
      }
      else
      {
        allocEnv(bindings[index + 1].getVar(), state.getTime(), getBindingEnv(), setOf());
        eval(bindings[index + 1].getValue(), state.getTime(), getBindingEnv(), new LetRecStackCont(getBindingEnv(),
            bindings, body, index + 1, cont));
      }
    }
  }

  private final class OperandStackCont extends StackCont
  {
    private final Set<Object> eoperators;
    private final Set<Object>[] eoperands;
    private final StackCont cont;
    private final Application application;
    private final Node[] operands;
    private final int index;

    public OperandStackCont(BindingEnv bindingEnv, Node[] operands, int index, Set<Object> eoperators,
        Set<Object>[] eoperands, Application application, StackCont cont)
    {
      super("operand" + index, cont, bindingEnv);
      this.eoperators = eoperators;
      this.eoperands = deepCopy(eoperands);
      this.cont = cont;
      this.application = application;
      this.operands = operands;
      this.index = index;
    }

    private Set<Object>[] deepCopy(Set<Object>[] eoperands2)
    {
      Set<Object>[] newArray = new Set[eoperands2.length];
      for (int i = 0; i < eoperands2.length; i++)
      {
        if (eoperands2[i] != null)
        {
          newArray[i] = new HashSet<Object>(eoperands2[i]);
        }
      }
      return newArray;
    }

    public void call(State eoperand)
    {
      eoperands[index] = eoperand.getValues();
      if (index == operands.length - 1)
      {
        apply(application, eoperators, eoperands, getBindingEnv(), eoperand.getTime(), cont);
      }
      else
      {
        eval(operands[index + 1], eoperand.getTime(), getBindingEnv(), new OperandStackCont(getBindingEnv(), operands, index + 1,
            eoperators, eoperands, application, cont));
      }
    }
  }

  private int k;
  private boolean gc;
  private Map<Object, AbstractValue> env = new HashMap<Object, AbstractValue>();
  private Map<Application, Set<AbstractVar<Time>>> reads = new HashMap<Application, Set<AbstractVar<Time>>>();
  private Map<Application, Set<AbstractVar<Time>>> writes = new HashMap<Application, Set<AbstractVar<Time>>>();
  private Map<Application, Set<CallingContext>> callingContexts = new HashMap<Application, Set<CallingContext>>();
  private Set<State> result = new LinkedHashSet<State>();
  private int steps;
  private VarPointerAnalysis varPointerAnalysis;

  public StatefulIpdAnalyzer(int k, boolean gc, VarPointerAnalysis varPointerAnalysis)
  {
    super();
    this.k = k;
    this.gc = gc;
    this.varPointerAnalysis = varPointerAnalysis;
  }

  public Void analyze(Node node)
  {
    BindingEnv bindingEnv = new BindingEnv();
    eval(node, new Time(), bindingEnv, new StackCont("exit", null, bindingEnv)
    {
      public void call(State state)
      {
        result.add(state);
      }
    });
    return null;
  }

  public Set<State> getResult()
  {
    return result;
  }

  public Map<Application, Set<AbstractVar<Time>>> getReads()
  {
    return reads;
  }

  public Map<Application, Set<AbstractVar<Time>>> getWrites()
  {
    return writes;
  }

  public void eval(Node node, final Time time, final BindingEnv bindingEnv, final StackCont cont)
  {
    System.out.println(++steps + " eval " + node + " " + time + " " + bindingEnv.toShortString() + " " + cont);
    if (gc)
    {
      gc(bindingEnv, cont);
    }
    if (steps == 87)
    {
      System.out.println("debug");
    }
    switch (node.type())
    {
      case LITERAL:
      {
        Literal literal = (Literal) node;
        cont.call(new State(setOf(literal.getValue()), time));
        break;
      }
      case DEFINE:
      {
        final Define define = (Define) node;
        eval(define.getValue(), time, bindingEnv, new StackCont("define", cont, bindingEnv)
        {
          public void call(State state)
          {
            allocEnv(define.getVar(), state.getTime(), bindingEnv, state.getValues());
            cont.call(new State(setOf(Void.TYPE), state.getTime()));
          }
        });
        break;
      }
      case REF:
      {
        Ref ref = (Ref) node;
        Var var = varPointerAnalysis.getVarRead(ref);
        if (var == null)
        {
          Object primitive = Primitives.get(ref.getName());
          if (primitive == null)
          {
            throw new StremeException("undefined reference " + ref);
          }
          cont.call(new State(setOf(primitive), time));
          break;
        }
        // register read dependency
        StackCont currentCont = cont;
        Time bindingTime = lookupAddress(var, bindingEnv).getContext();
        while (currentCont != null)
        {
          Set<Application> applicationMarks = currentCont.getApplicationMarks();
          for (Application application : applicationMarks)
          {
            Set<AbstractVar<Time>> reads = StatefulIpdAnalyzer.this.reads.get(application);
            if (reads == null)
            {
              reads = new HashSet<AbstractVar<Time>>();
              StatefulIpdAnalyzer.this.reads.put(application, reads);
            }
            reads.add(new AbstractVar<Time>(var, bindingTime));
          }
          currentCont = currentCont.getPrevious();
        }
        cont.call(new State(lookupEnv(var, bindingEnv), time));
        break;
      }
      case LET:
      {
        final Let let = (Let) node;
        switch (let.getKind())
        {
          case LET:
          {
            BindingEnv extendedBindingEnv = new BindingEnv(bindingEnv);
            Binding[] bindings = let.getBindings();
            eval(bindings[0].getValue(), time, extendedBindingEnv, new LetStackCont(false, extendedBindingEnv,
                bindings, let.getBody(), 0, cont));
            break;
          }
          case LETSTAR:
          {
            BindingEnv extendedBindingEnv = new BindingEnv(bindingEnv);
            Binding[] bindings = let.getBindings();
            eval(bindings[0].getValue(), time, extendedBindingEnv, new LetStackCont(true, extendedBindingEnv, bindings,
                let.getBody(), 0, cont));
            break;
          }
          case LETREC:
          {
            final BindingEnv extendedBindingEnv = new BindingEnv(bindingEnv);
            Binding[] bindings = let.getBindings();
            allocEnv(bindings[0].getVar(), time, extendedBindingEnv, setOf()); // note: let vars are bound with time *after* evaluating binding exp
            eval(let.getValue(0), time, extendedBindingEnv, new LetRecStackCont(extendedBindingEnv, bindings, let.getBody(), 0, cont));
            break;
          }
          default:
            throw new StremeException("cannot handle " + let.toShortString());
        }
        break;
      }
      case IF:
      {
        final If ff = (If) node;
        eval(ff.getCondition(), time, bindingEnv, new StackCont("if", cont, bindingEnv)
        {
          public void call(State state)
          {
            Set<Object> values = state.getValues();
            if (values.equals(setOf(Boolean.FALSE)))
            {
              eval(ff.getAlternate(), state.getTime(), bindingEnv, cont);
            }
            else if (values.contains(Boolean.FALSE))
            {
              Time time2 = state.getTime().tick(ff.getConsequent().getTag(), k);
              eval(ff.getConsequent(), time2, bindingEnv, cont);
              Time time3 = state.getTime().tick(ff.getAlternate().getTag(), k);
              eval(ff.getAlternate(), time3, bindingEnv, cont);
            }
            else
            {
              eval(ff.getConsequent(), state.getTime(), bindingEnv, cont);
            }
          }
        });
        break;
      }
      case LAMBDA:
      {
        final Lambda lambda = (Lambda) node;
        Procedure closure = new Closure(this, lambda, bindingEnv);
        cont.call(new State(setOf(closure), time));
        break;
      }
      case APPLICATION:
      {
        final Application application = (Application) node;
        // final Time time2 = time.tick(application.getTag(), k);
        eval(application.getOperator(), time, bindingEnv, new StackCont("operator ("
            + application.getOperator().toShortString() + ")", cont, bindingEnv)
        {
          public void call(final State operator)
          {
            final Node[] operands = application.getOperands();
            if (operands.length == 0)
            {
              apply(application, operator.getValues(), new Set[0], bindingEnv, operator.getTime(), cont);
            }
            else if (operands.length == 1)
            {
              eval(operands[0], operator.getTime(), bindingEnv, new StackCont("singleOperand", cont, bindingEnv)
              {
                public void call(State eoperand)
                {
                  Set<Object>[] eoperands = new Set[operands.length];
                  eoperands[0] = eoperand.getValues();
                  apply(application, operator.getValues(), eoperands, bindingEnv, eoperand.getTime(), cont);
                }
              });
            }
            else
            {
              eval(operands[0], operator.getTime(), bindingEnv,
                  new OperandStackCont(bindingEnv, operands, 0, operator.getValues(), new Set[operands.length],
                      application, cont));
            }
          }
        });
        break;
      }
      case BEGIN:
      {
        Begin begin = (Begin) node;
        final Node[] exps = begin.getExps();
        if (exps.length == 0)
        {
          cont.call(new State(setOf(Void.TYPE), time));
        }
        else if (exps.length == 1)
        {
          eval(exps[0], time, bindingEnv, cont);
        }
        else
        {
          eval(exps[0], time, bindingEnv, new BeginStackCont(bindingEnv, exps, 0, cont));
        }
        break;
      }
      case SETVAR:
      {
        final SetVar setVar = (SetVar) node;
        eval(setVar.getValue(), time, bindingEnv, new StackCont("set!", cont, bindingEnv)
        {
          public void call(State state)
          {
            Var var = varPointerAnalysis.getVarWritten(setVar);
            if (var == null)
            {
              throw new StremeException("undefined variable " + setVar.getVar());
            }
            Time bindingTime = updateEnv(var, bindingEnv, state.getValues()).getContext();
            // register write dependency
            StackCont currentCont = cont;
            while (currentCont != null)
            {
              Set<Application> applicationMarks = currentCont.getApplicationMarks();
              for (Application application : applicationMarks)
              {
                Set<AbstractVar<Time>> writes = StatefulIpdAnalyzer.this.writes.get(application);
                if (writes == null)
                {
                  writes = new HashSet<AbstractVar<Time>>();
                  StatefulIpdAnalyzer.this.writes.put(application, writes);
                }
                writes.add(new AbstractVar<Time>(var, bindingTime));
              }
              currentCont = currentCont.getPrevious();
            }
            cont.call(new State(setOf(Void.TYPE), state.getTime()));
          }
        });
        break;
      }
      default:
        throw new StremeException("cannot handle " + node);
    }
  }

  private Set<Object> setOf(Object... els)
  {
    Set<Object> set = new LinkedHashSet<Object>();
    for (Object el : els)
    {
      set.add(el);
    }
    return set;
  }

  private void apply(final Application application, final Set<Object> operators, final Set<Object>[] eoperands,
      final BindingEnv bindingEnv, Time time, final StackCont cont)
  {
    cont.markApplication(application);
    Time time2 = time.tick(application.getTag(), k);
    for (Object op : operators)
    {
      Procedure procedure = (Procedure) op;
      CallingContext context = new CallingContext(time2, procedure, eoperands);
      Set<CallingContext> contexts = callingContexts.get(application);
      if (contexts == null)
      {
        contexts = new LinkedHashSet<CallingContext>();
        callingContexts.put(application, contexts);
      }
      if (!contexts.contains(context))
      {
        contexts.add(context);
        // cont.mark(new AbstractProcedure((Procedure) op, application));
        procedure.apply(eoperands, time2, bindingEnv, cont);
      }
      else
      {
        System.out.println("\t" + application + " calling context " + context + " already in " + contexts);
      }
    }
  }


  public void allocEnv(Var var, Time time, BindingEnv bindingEnv, Set<Object> val)
  {
    AbstractVar<Time> abstractVar = new AbstractVar<Time>(var, time);
    bindingEnv.extend(var.getName(), abstractVar);
    AbstractValue current = env.get(abstractVar);
    if (current == null)
    {
      current = new AbstractValue(val);
      env.put(abstractVar, current);
//      System.out.println("alloc " + abstractVar + " -> " + current);
    }
    else
    {
      // reallocing: weak
      current.weakUpdate(val);  
    }
  }

  private AbstractVar<Time> updateEnv(Var var, BindingEnv bindingEnv, Set<Object> val)
  {
    AbstractVar<Time> abstractVar = lookupAddress(var, bindingEnv);
    AbstractValue current = env.get(abstractVar);
    if (current == null)
    {
      throw new StremeException("cannot lookup " + abstractVar + " in " + env);
    }
    // update: keep cardinality if fresh
    current.update(val);
    return abstractVar;
  }

  private static AbstractVar<Time> lookupAddress(Var var, BindingEnv bindingEnv)
  {
    AbstractVar<Time> abstractVar = bindingEnv.lookup(var.getName());
    if (abstractVar == null)
    {
      throw new StremeException("cannot lookup " + var + " in " + bindingEnv);
    }
    return abstractVar;
  }

  private Set<Object> lookupEnv(Var var, BindingEnv bindingEnv)
  {
    AbstractVar<Time> abstractVar = lookupAddress(var, bindingEnv);
    AbstractValue abstractValue = env.get(abstractVar);
    if (abstractValue == null)
    {
      throw new StremeException("no abstract value for " + abstractVar);
    }
    return abstractValue.values(Object.class);
  }

  public Set<Object> getMonoValues(Var var)
  {
    Set<Object> mono = new HashSet<Object>();
    for (Object address : env.keySet())
    {
      if (address instanceof AbstractVar)
      {
        AbstractVar abstractVar = (AbstractVar) address;
        if (abstractVar.getVar().equals(var))
        {
          AbstractValue av = env.get(abstractVar);
          mono.addAll(av.mono(Object.class));
        }
      }
    }
    return mono;
  }

  public Set<Object> getValues(Var var)
  {
    Set<Object> mono = new HashSet<Object>();
    for (Object address : env.keySet())
    {
      if (address instanceof AbstractVar)
      {
        AbstractVar abstractVar = (AbstractVar) address;
        if (abstractVar.getVar().equals(var))
        {
          AbstractValue av = env.get(abstractVar);
          mono.addAll(av.values(Object.class));
        }
      }
    }
    return mono;
  }
  
  public Set<Object> reachable(Set<? extends Object> roots)
  {
    Set<Object> reachable = new HashSet<Object>();
    Deque<Object> todo = new ArrayDeque<Object>(roots);
    while (!todo.isEmpty())
    {
      Object address = todo.pop();
      if (reachable.contains(address))
      {
        continue;
      }
      reachable.add(address);
      AbstractValue abstractValue = env.get(address);
      for (Object value : abstractValue.values(Object.class))
      {
        Set<AbstractVar<Time>> touches;
        if (value instanceof Closure)
        {
          touches = ((Closure) value).getBindingEnv().touches();
        }
        else
        {
          touches = Collections.emptySet();
        }
        todo.addAll(touches);
      }
    }
    return reachable;
  }
  
  private void gc(BindingEnv bindingEnv, StackCont cont)
  {
    Set<Object> reachable = reachable(bindingEnv.touches());
    StackCont current = cont;
    while (current != null)
    {
      Set<? extends Object> roots = current.getBindingEnv().touches();
      reachable.addAll(reachable(roots));
      current = current.getPrevious();
    }
  //  System.out.println("reachable: " + reachable);
      Iterator<Object> iter = env.keySet().iterator();
      while (iter.hasNext())
      {
        Object address = iter.next();
        if (!reachable.contains(address))
        {
//          System.out.println("resetting " + abstractVar);
          env.get(address).reset();
        }
      }
  }

  public static void main(String[] args)
  {
    // String source = "(letrec ((fib (lambda (n) (if (< n 2) n (+ (fib (- n 2)) (fib (- n 1))))))) (fib 4))";
    // String source = "(begin (define z 123) (define f (lambda () z)) (f) (f))";
    // String source = "(begin (define counter (lambda (c) (if (zero? c) 'boem (counter (- c 1))))) (counter 2))";
    // String source =
    // "(letrec ((fib (lambda (n) (if (< n 2) n (let ((a (fib (- n 1))) (b (fib (- n 2)))) (+ a b)))))) (fib 3))";
    // String source = "(let* ((z 0) (writez (lambda () (set! z 123))) (readz (lambda () z))) (cons (writez) (readz)))";
//    String source = "(begin (define f (lambda (x) x)) (f 123))";
    String source = "(begin (define appender (lambda (h a b) (append (h a) (h b)))) (define lister (lambda (g) (lambda (x) (list (g x))))) (define square (lambda (x) (* x x))) (appender (lister square) 42 43))";
    Parser2 parser = new Parser2();
    Object data = parser.parse(source);
    StremeDataCompiler compiler = new StremeDataCompiler();
    Node ast = compiler.compile(data);
    ParentAnalysis parentAnalysis = new ParentAnalyzer().analyze(ast);
    VarPointerAnalysis varPointerAnalysis = new VarPointerAnalyzer(parentAnalysis).analyze(ast);
    StatefulIpdAnalyzer ipda = new StatefulIpdAnalyzer(20, true, varPointerAnalysis);
    ipda.analyze(ast);
    System.out.println(TagPrinter.print(ast));
    System.out.println(ipda.getResult());
    System.out.println(ipda.getReads());
    System.out.println(ipda.getWrites());
    
    NdAnalysisStreme querier = new NdAnalysisStreme(ast);
    Application app = (Application) querier.queryNode("(an-application-with-operator (a-ref-with-name 'append))");
    System.out.println(ipda.getReads().get((Application) app.getOperands()[0]));
    System.out.println(ipda.getReads().get((Application) app.getOperands()[1]));
    System.out.println(ipda.getWrites().get((Application) app.getOperands()[0]));
    System.out.println(ipda.getWrites().get((Application) app.getOperands()[1]));

  }
}
