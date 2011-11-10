package streme.lang.eval.nd;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Random;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;

import streme.lang.StremeException;
import streme.lang.TCont;
import streme.lang.ast.Literal;
import streme.lang.data.BigRational;
import streme.lang.data.DataUnifier;
import streme.lang.data.Lst;
import streme.lang.data.Null;
import streme.lang.data.Pair;
import streme.lang.data.Sym;
import streme.lang.eval.ConstructorUtils;
import streme.lang.eval.MapEnv;
import streme.lang.eval.MethodUtils;
import streme.lang.eval.Primitives;

public class NdStremePrimitives
{
  private static ClassLoader classLoader = NdStremePrimitives.class.getClassLoader();

  public static void setClassLoader(ClassLoader cl)
  {
    classLoader = cl;
  }
  
  public static void loadPrimitives(MapEnv env, NdStremeContext context)
  {
    /* testing primitives */
    env.add(new Sym("fixed-random-list"), new FixedRandomList());
    /* Java interop */
    env.add(new Sym("new"), new New());
    env.add(new Sym("class-for-name"), new ClassForName());
    env.add(new Sym("invoke"), new Invoke());
    env.add(new Sym("invoke-static"), new InvokeStatic());
    env.add(new Sym("proxy"), new Proxy(context));
    env.add(new Sym("unbound->false"), new Unbound2False());
    env.add(new Sym("iterable?"), new IterableP());
    /* Streme primitives */
    env.add(new Sym("error"), new Error());
    env.add(new Sym("improper-list"), new ImproperList());
    env.add(new Sym("primitive?"), new PrimitiveP());
    env.add(new Sym("load"), new Load(context));
    env.add(new Sym("touch"), new Touch());
    env.add(new Sym("current-environment"), new CurrentEnvironment());
    env.add(new Sym("unify"), new Unify());
    env.add(new Sym("sleep"), new Sleep());
    env.add(new Sym("amb-thunks"), new AmbThunks());
    env.add(new Sym("amb-list-thunk"), new AmbListThunk());
    env.add(new Sym("exists-thunk?"), new ExistsPThunk());
    // env.put(new Sym("serializer"), new Serializer_());
    // env.put(new Sym("serial"), new Serial());
    // env.put(new Sym("yield"), new Yield());
    /* R5RS primitives */
    env.add(new Sym("call-with-current-continuation"), new CallWithCurrentContinuation());
    env.add(new Sym("call/cc"), new CallWithCurrentContinuation());
    env.add(new Sym("interaction-environment"), new InteractionEnvironment(context));
    env.add(new Sym("read"), new Read(context));
    env.add(new Sym("newline"), new Newline());
    env.add(new Sym("display"), new Display());
    env.add(new Sym("apply"), new Apply());
    env.add(new Sym("eval"), new Eval(context));
    env.add(new Sym("string<?"), new StringLessThanP());
    env.add(new Sym("string-append"), new StringAppend());
    env.add(new Sym("string->symbol"), new String2Symbol());
    env.add(new Sym("symbol->string"), new Symbol2String());
    env.add(new Sym("string-ref"), new StringRef());
    env.add(new Sym("number->string"), new Number2String());
    env.add(new Sym("list->vector"), new List2Vector());
    env.add(new Sym("vector->list"), new Vector2List());
    env.add(new Sym("->f"), new ToF());
    env.add(new Sym("number?"), new NumberP());
    env.add(new Sym("symbol?"), new SymbolP());
    env.add(new Sym("pair?"), new PairP());
    env.add(new Sym("list?"), new ListP());
    env.add(new Sym("assoc"), new Assoc());
    env.add(new Sym("assq"), new Assq());
    env.add(new Sym("memq"), new Memq());
    env.add(new Sym("memv"), new Memv());
    env.add(new Sym("member"), new Member());
    env.add(new Sym("append"), new Append());
    env.add(new Sym("map||"), new PMap(context.executor()));
    env.add(new Sym("map"), new Map());
    env.add(new Sym("for-each"), new ForEach());
    env.add(new Sym("list"), new List());
    env.add(new Sym("cdr"), new Cdr());
    env.add(new Sym("car"), new Car());
    env.add(new Sym("cadr"), new Cadr());
    env.add(new Sym("cdar"), new Cdar());
    env.add(new Sym("caar"), new Caar());
    env.add(new Sym("caadr"), new Caadr());
    env.add(new Sym("caddr"), new Caddr());
    env.add(new Sym("cadddr"), new Cadddr());
    env.add(new Sym("cadadr"), new Cadadr());
    env.add(new Sym("cddr"), new Cddr());
    env.add(new Sym("cdddr"), new Cdddr());
    env.add(new Sym("list-ref"), new ListRef());
    env.add(new Sym("reverse"), new Reverse());
    env.add(new Sym("equal?"), new EqualP());
    env.add(new Sym("eqv?"), new EqvP());
    env.add(new Sym("eq?"), new EqP());
    env.add(new Sym("length"), new Length());
    env.add(new Sym("null?"), new NullP());
    env.add(new Sym("vector-ref"), new VectorRef());
    env.add(new Sym("make-vector"), new MakeVector());
    env.add(new Sym("vector"), new Vector());
    env.add(new Sym("zero?"), new IZeroP());
    env.add(new Sym("Izero?"), new IIZeroP());
    env.add(new Sym("even?"), new IEvenP());
    env.add(new Sym("odd?"), new IOddP());
    env.add(new Sym("negative?"), new NegativeP());
    env.add(new Sym("Ieven?"), new IIEvenP());
    env.add(new Sym("Iodd?"), new IIOddP());
    env.add(new Sym("cons"), new Cons());
    env.add(new Sym("not"), new Not());
    env.add(new Sym("="), new Equals());
    env.add(new Sym(">"), new GreaterThan());
    env.add(new Sym(">="), new GreaterThanOrEquals());
    env.add(new Sym("<"), new LessThan());
    env.add(new Sym("<="), new LessThanOrEquals());
    env.add(new Sym("remainder"), new IRemainder());
    env.add(new Sym("modulo"), new IModulo());
    env.add(new Sym("quotient"), new IQuotient());
    env.add(new Sym("-"), new Minus());
    env.add(new Sym("+"), new Plus());
    env.add(new Sym("*"), new Multiply());
    env.add(new Sym("+i"), new IPlus());
    env.add(new Sym("-i"), new IMinus());
    env.add(new Sym("*i"), new IMultiply());
    env.add(new Sym("/i"), new IDivide());
    env.add(new Sym("/I"), new IIDivide());
    env.add(new Sym("-I"), new IIMinus());
    env.add(new Sym("+I"), new IIPlus());
    env.add(new Sym("*I"), new IIMultiply());
    env.add(new Sym("/f"), new FDivide());
    env.add(new Sym("-f"), new FMinus());
    env.add(new Sym("+f"), new FPlus());
    env.add(new Sym("*f"), new FMultiply());
    env.add(new Sym("sin"), new Sin());
    env.add(new Sym("cos"), new Cos());
    env.add(new Sym("atan"), new Atan());
    env.add(new Sym("sqrt"), new Sqrt());
    env.add(new Sym("abs"), new Abs());
    // env.put(new Sym("F/"), new FFDivide());
    // env.put(new Sym("F-"), new FFMinus());
    // env.put(new Sym("F+"), new FFPlus());
    // env.put(new Sym("F*"), new FFMultiply());
  }

  public static final Integer IZERO = Integer.valueOf(0);

  public static final class AmbListThunk extends Procedure
  {
    public Callable<Callable> apply1(Object operand, MapEnv env, final TSuccess cont, final TCont fail)
    {
      final java.util.List<Object> r = new ArrayList<Object>(); // not immutable: unintended behaviour when using call/cc
      return ((Procedure) operand).apply0(env, new TSuccess()
      {
        public Callable<Callable> call(Object value, TCont fail2)
        {
          r.add(value);
          return fail2.call(null);
        }
      }, new TCont()
      {
        public Callable<Callable> call(Object value)
        {
          return cont.call(Lst.valueOf(r), fail);
        }
      });
    }
  }

  public static final class ExistsPThunk extends Procedure
  {
    public Callable<Callable> apply1(Object operand, MapEnv env, final TSuccess cont, final TCont fail)
    {
      return ((Procedure) operand).apply0(env, new TSuccess()
      {
        public Callable<Callable> call(Object value, TCont fail2)
        {
          return cont.call(Boolean.TRUE, fail);
        }
      }, new TCont()
      {
        public Callable<Callable> call(Object value)
        {
          return cont.call(Boolean.FALSE, fail);
        }
      });
    }
  }

  public static final class AmbThunks extends Procedure
  {
    public Callable<Callable> apply0(MapEnv env, TSuccess cont, TCont fail)
    {
      return fail.call(null);
    }

    public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
    {
      return ((Procedure) operand).apply0(env, cont, fail);
    }

    public Callable<Callable> apply2(Object operand1, final Object operand2, final MapEnv env, final TSuccess cont,
        final TCont fail)
    {
      return ((Procedure) operand1).apply0(env, cont, new TCont()
      {
        public Callable<Callable> call(Object value)
        {
          return ((Procedure) operand2).apply0(env, cont, fail);
        }
      });
    }

    public Callable<Callable> apply3(Object operand1, final Object operand2, final Object operand3, final MapEnv env,
        final TSuccess cont, final TCont fail)
    {
      return ((Procedure) operand1).apply0(env, cont, new TCont()
      {
        public Callable<Callable> call(Object value)
        {
          return ((Procedure) operand2).apply0(env, cont, new TCont()
          {
            public Callable<Callable> call(Object value)
            {
              return ((Procedure) operand3).apply0(env, cont, fail);
            }
          });
        }
      });
    }

    public Callable<Callable> applyN(final Object[] operands, MapEnv env, final TSuccess cont, final TCont fail)
    {
      return ambifyThunksN(operands, env, cont, fail);
    }
  }

  public static Callable<Callable> ambifyValues(final Object[] operands, final MapEnv env, final TSuccess cont,
      final TCont fail)
  {
    if (operands.length == 0)
    {
      return fail.call(null);
    }
    class AmbValuesCont extends TCont
    {
      private int i;

      public AmbValuesCont(int i)
      {
        super();
        this.i = i;
      }

      public Callable<Callable> call(Object value)
      {
        return cont.call(operands[i], i == operands.length - 1 ? fail : new AmbValuesCont(i + 1));
      }
    }
    return cont.call(operands[0], operands.length == 1 ? fail : new AmbValuesCont(1));
  }

  /**
   * assert operands.length > 1
   */
  public static Callable<Callable> ambifyThunksN(final Object[] operands, final MapEnv env, final TSuccess cont,
      final TCont fail)
  {
    class AmbCont extends TCont
    {
      private int i;

      public AmbCont(int i)
      {
        super();
        this.i = i;
      }

      public Callable<Callable> call(Object value)
      {
        return ((Procedure) operands[i]).apply0(env, cont, i == operands.length - 1 ? fail : new AmbCont(i + 1));
      }
    }
    return ((Procedure) operands[0]).apply0(env, cont, new AmbCont(1));
  }

  public static final class CallWithCurrentContinuation extends Procedure
  {
    public Callable<Callable> apply1(Object calledWithCc, final MapEnv env, final TSuccess cont, TCont fail)
    {
      Procedure calledWithCurrentContinuation = (Procedure) calledWithCc;
      return calledWithCurrentContinuation.apply1(new Procedure()
      {
        public Callable<Callable> apply1(Object operand, MapEnv e, TSuccess k, TCont fail)
        {
          return cont.call(operand, fail);
        }
      }, env, cont, fail);
    }
  }

  public static final class IIPlus extends Procedure
  {
    public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(operand, fail);
    }

    public Callable<Callable> apply2(Object operand1, Object operand2, MapEnv env, TSuccess cont, TCont fail)
    {
      BigInteger i = (BigInteger) operand1;
      BigInteger j = (BigInteger) operand2;
      return cont.call(i.add(j), fail);
    }

    public Callable<Callable> applyN(Object[] operands, MapEnv env, TSuccess cont, TCont fail)
    {
      BigInteger r = (BigInteger) operands[0];
      for (int i = 1; i < operands.length; i++)
      {
        r = r.add((BigInteger) operands[i]);
      }
      return cont.call(r, fail);
    }
  }

  public static final class IPlus extends Procedure
  {
    public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(operand, fail);
    }

    public Callable<Callable> apply2(Object operand1, Object operand2, MapEnv env, TSuccess cont, TCont fail)
    {
      int i = (Integer) operand1;
      int j = (Integer) operand2;
      return cont.call(i + j, fail);
    }

    public Callable<Callable> apply3(Object operand1, Object operand2, Object operand3, MapEnv env, TSuccess cont,
        TCont fail)
    {
      int i = (Integer) operand1;
      int j = (Integer) operand2;
      int k = (Integer) operand3;
      return cont.call(i + j + k, fail);
    }

    public Callable<Callable> applyN(Object[] operands, MapEnv env, TSuccess cont, TCont fail)
    {
      int r = (Integer) operands[0];
      for (int i = 1; i < operands.length; i++)
      {
        r += (Integer) operands[i];
      }
      return cont.call(r, fail);
    }
  }

  public static final class Plus extends Procedure
  {
    public static Object plus(Object operand1, Object operand2)
    {
      Class c1 = operand1.getClass();
      Class c2 = operand2.getClass();
      if (c1 == Integer.class)
      {
        if (c2 == Integer.class)
        {
          int i = (Integer) operand1;
          int j = (Integer) operand2;
          return i + j;
        }
      }
      if (c1 == Double.class)
      {
        if (c2 == Double.class)
        {
          double i = (Double) operand1;
          double j = (Double) operand2;
          return i + j;
        }
      }
      if (c1 == Long.class)
      {
        if (c2 == Long.class)
        {
          long i = (Long) operand1;
          long j = (Long) operand2;
          return i + j;
        }
      }
      throw new IllegalArgumentException("(+ " + operand1 + " " + operand2 + ")");
    }

    public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(operand, fail);
    }

    public Callable<Callable> apply2(Object operand1, Object operand2, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(plus(operand1, operand2), fail);
    }

    public Callable<Callable> apply3(Object operand1, Object operand2, Object operand3, MapEnv env, TSuccess cont,
        TCont fail)
    {
      int i = (Integer) operand1;
      int j = (Integer) operand2;
      int k = (Integer) operand3;
      return cont.call(i + j + k, fail);
    }

    public Callable<Callable> applyN(Object[] operands, MapEnv env, TSuccess cont, TCont fail)
    {
      int r = (Integer) operands[0];
      for (int i = 1; i < operands.length; i++)
      {
        r += (Integer) operands[i];
      }
      return cont.call(r, fail);
    }
  }

  public static final class FPlus extends Procedure
  {
    public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(operand, fail);
    }

    public Callable<Callable> apply2(Object operand1, Object operand2, MapEnv env, TSuccess cont, TCont fail)
    {
      double i = (Double) operand1;
      double j = (Double) operand2;
      return cont.call(i + j, fail);
    }

    public Callable<Callable> apply3(Object operand1, Object operand2, Object operand3, MapEnv env, TSuccess cont,
        TCont fail)
    {
      double i = (Double) operand1;
      double j = (Double) operand2;
      double k = (Double) operand3;
      return cont.call(i + j + k, fail);
    }

    public Callable<Callable> applyN(Object[] operands, MapEnv env, TSuccess cont, TCont fail)
    {
      double r = (Double) operands[0];
      for (int i = 1; i < operands.length; i++)
      {
        r += (Double) operands[i];
      }
      return cont.call(r, fail);
    }
  }

  public static final class FMinus extends Procedure
  {
    public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(operand, fail);
    }

    public Callable<Callable> apply2(Object operand1, Object operand2, MapEnv env, TSuccess cont, TCont fail)
    {
      double i = (Double) operand1;
      double j = (Double) operand2;
      return cont.call(i - j, fail);
    }

    public Callable<Callable> apply3(Object operand1, Object operand2, Object operand3, MapEnv env, TSuccess cont,
        TCont fail)
    {
      double i = (Double) operand1;
      double j = (Double) operand2;
      double k = (Double) operand3;
      return cont.call(i - j - k, fail);
    }

    public Callable<Callable> applyN(Object[] operands, MapEnv env, TSuccess cont, TCont fail)
    {
      double r = (Double) operands[0];
      for (int i = 1; i < operands.length; i++)
      {
        r -= (Double) operands[i];
      }
      return cont.call(r, fail);
    }
  }

  public static final class FMultiply extends Procedure
  {
    public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(operand, fail);
    }

    public Callable<Callable> apply2(Object operand1, Object operand2, MapEnv env, TSuccess cont, TCont fail)
    {
      double i = (Double) operand1;
      double j = (Double) operand2;
      return cont.call(i * j, fail);
    }

    public Callable<Callable> apply3(Object operand1, Object operand2, Object operand3, MapEnv env, TSuccess cont,
        TCont fail)
    {
      double i = (Double) operand1;
      double j = (Double) operand2;
      double k = (Double) operand3;
      return cont.call(i * j * k, fail);
    }

    public Callable<Callable> applyN(Object[] operands, MapEnv env, TSuccess cont, TCont fail)
    {
      double r = (Double) operands[0];
      for (int i = 1; i < operands.length; i++)
      {
        r *= (Double) operands[i];
      }
      return cont.call(r, fail);
    }
  }

  public static final class FDivide extends Procedure
  {
    public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(operand, fail);
    }

    public Callable<Callable> apply2(Object operand1, Object operand2, MapEnv env, TSuccess cont, TCont fail)
    {
      double i = (Double) operand1;
      double j = (Double) operand2;
      return cont.call(i / j, fail);
    }

    public Callable<Callable> apply3(Object operand1, Object operand2, Object operand3, MapEnv env, TSuccess cont,
        TCont fail)
    {
      double i = (Double) operand1;
      double j = (Double) operand2;
      double k = (Double) operand3;
      return cont.call(i / j / k, fail);
    }

    public Callable<Callable> applyN(Object[] operands, MapEnv env, TSuccess cont, TCont fail)
    {
      double r = (Double) operands[0];
      for (int i = 1; i < operands.length; i++)
      {
        r /= (Double) operands[i];
      }
      return cont.call(r, fail);
    }
  }

  public static final class Minus extends Procedure
  {
    public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(operand, fail);
    }

    public Callable<Callable> apply2(Object operand1, Object operand2, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(minus(operand1, operand2), fail);
    }

    public static Object minus(Object operand1, Object operand2)
    {
      Class c1 = operand1.getClass();
      Class c2 = operand2.getClass();
      if (c1 == Integer.class)
      {
        if (c2 == Integer.class)
        {
          int i = (Integer) operand1;
          int j = (Integer) operand2;
          return i - j;
        }
      }
      if (c1 == Double.class)
      {
        if (c2 == Double.class)
        {
          double i = (Double) operand1;
          double j = (Double) operand2;
          return i - j;
        }
      }
      if (c1 == Long.class)
      {
        if (c2 == Long.class)
        {
          long i = (Long) operand1;
          long j = (Long) operand2;
          return i - j;
        }
      }
      throw new IllegalArgumentException("(- " + operand1 + " " + operand2 + ")");
    }

    public Callable<Callable> apply3(Object operand1, Object operand2, Object operand3, MapEnv env, TSuccess cont,
        TCont fail)
    {
      int i = (Integer) operand1;
      int j = (Integer) operand2;
      int k = (Integer) operand3;
      return cont.call(i - j - k, fail);
    }

    public Callable<Callable> applyN(Object[] operands, MapEnv env, TSuccess cont, TCont fail)
    {
      int r = (Integer) operands[0];
      for (int i = 1; i < operands.length; i++)
      {
        r -= (Integer) operands[i];
      }
      return cont.call(r, fail);
    }
  }

  public static final class IMinus extends Procedure
  {
    public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(operand, fail);
    }

    public Callable<Callable> apply2(Object operand1, Object operand2, MapEnv env, TSuccess cont, TCont fail)
    {
      int i = (Integer) operand1;
      int j = (Integer) operand2;
      return cont.call(i - j, fail);
    }

    public Callable<Callable> apply3(Object operand1, Object operand2, Object operand3, MapEnv env, TSuccess cont,
        TCont fail)
    {
      int i = (Integer) operand1;
      int j = (Integer) operand2;
      int k = (Integer) operand3;
      return cont.call(i - j - k, fail);
    }

    public Callable<Callable> applyN(Object[] operands, MapEnv env, TSuccess cont, TCont fail)
    {
      int r = (Integer) operands[0];
      for (int i = 1; i < operands.length; i++)
      {
        r -= (Integer) operands[i];
      }
      return cont.call(r, fail);
    }
  }

  public static final class IMultiply extends Procedure
  {
    public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(operand, fail);
    }

    public Callable<Callable> apply2(Object operand1, Object operand2, MapEnv env, TSuccess cont, TCont fail)
    {
      int i = (Integer) operand1;
      int j = (Integer) operand2;
      return cont.call(i * j, fail);
    }

    public Callable<Callable> apply3(Object operand1, Object operand2, Object operand3, MapEnv env, TSuccess cont,
        TCont fail)
    {
      int i = (Integer) operand1;
      int j = (Integer) operand2;
      int k = (Integer) operand3;
      return cont.call(i * j * k, fail);
    }

    public Callable<Callable> applyN(Object[] operands, MapEnv env, TSuccess cont, TCont fail)
    {
      int r = (Integer) operands[0];
      for (int i = 1; i < operands.length; i++)
      {
        r *= (Integer) operands[i];
      }
      return cont.call(r, fail);
    }
  }

  public static final class Multiply extends Procedure
  {
    public static Object multiply(Object operand1, Object operand2)
    {
      Class c1 = operand1.getClass();
      Class c2 = operand2.getClass();
      if (c1 == Integer.class)
      {
        if (c2 == Integer.class)
        {
          int i = (Integer) operand1;
          int j = (Integer) operand2;
          return i * j;
        }
      }
      if (c1 == Double.class)
      {
        if (c2 == Double.class)
        {
          double i = (Double) operand1;
          double j = (Double) operand2;
          return i * j;
        }
      }
      if (c1 == Long.class)
      {
        if (c2 == Long.class)
        {
          long i = (Long) operand1;
          long j = (Long) operand2;
          return i * j;
        }
      }
      throw new IllegalArgumentException("(+ " + operand1 + " " + operand2 + ")");
    }

    public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(operand, fail);
    }

    public Callable<Callable> apply2(Object operand1, Object operand2, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(multiply(operand1, operand2), fail);
    }

    public Callable<Callable> apply3(Object operand1, Object operand2, Object operand3, MapEnv env, TSuccess cont,
        TCont fail)
    {
      int i = (Integer) operand1;
      int j = (Integer) operand2;
      int k = (Integer) operand3;
      return cont.call(i * j * k, fail);
    }

    public Callable<Callable> applyN(Object[] operands, MapEnv env, TSuccess cont, TCont fail)
    {
      int r = (Integer) operands[0];
      for (int i = 1; i < operands.length; i++)
      {
        r *= (Integer) operands[i];
      }
      return cont.call(r, fail);
    }
  }

  public static final class IDivide extends Procedure
  {
    public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(operand, fail);
    }

    public Callable<Callable> apply2(Object operand1, Object operand2, MapEnv env, TSuccess cont, TCont fail)
    {
      int i = (Integer) operand1;
      int j = (Integer) operand2;
      return cont.call(i / j, fail);
    }

    public Callable<Callable> apply3(Object operand1, Object operand2, Object operand3, MapEnv env, TSuccess cont,
        TCont fail)
    {
      int i = (Integer) operand1;
      int j = (Integer) operand2;
      int k = (Integer) operand3;
      return cont.call(i / j / k, fail);
    }

    public Callable<Callable> applyN(Object[] operands, MapEnv env, TSuccess cont, TCont fail)
    {
      int r = (Integer) operands[0];
      for (int i = 1; i < operands.length; i++)
      {
        r /= (Integer) operands[i];
      }
      return cont.call(r, fail);
    }
  }

  public static final class Sin extends Procedure
  {
    public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(Math.sin((Double) operand), fail);
    }
  }

  public static final class Cos extends Procedure
  {
    public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(Math.cos((Double) operand), fail);
    }
  }

  public static final class Atan extends Procedure
  {
    public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(Math.atan((Double) operand), fail);
    }
  }

  public static final class Sqrt extends Procedure
  {
    public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(Math.sqrt((Double) operand), fail);
    }
  }

  public static final class Abs extends Procedure
  {
    public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(Math.abs((Integer) operand), fail);
    }
  }

  public static final class IRemainder extends Procedure
  {
    public Callable<Callable> apply2(Object operand1, Object operand2, MapEnv env, TSuccess cont, TCont fail)
    {
      int i = (Integer) operand1;
      int j = (Integer) operand2;
      return cont.call(i % j, fail);
    }
  }

  public static final class IIMultiply extends Procedure
  {
    public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(operand, fail);
    }

    public Callable<Callable> apply2(Object operand1, Object operand2, MapEnv env, TSuccess cont, TCont fail)
    {
      BigInteger i = (BigInteger) operand1;
      BigInteger j = (BigInteger) operand2;
      return cont.call(i.multiply(j), fail);
    }

    public Callable<Callable> applyN(Object[] operands, MapEnv env, TSuccess cont, TCont fail)
    {
      BigInteger r = (BigInteger) operands[0];
      for (int i = 1; i < operands.length; i++)
      {
        r = r.multiply((BigInteger) operands[i]);
      }
      return cont.call(r, fail);
    }
  }

  public static final class IIDivide extends Procedure
  {
    public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(operand, fail);
    }

    public Callable<Callable> apply2(Object operand1, Object operand2, MapEnv env, TSuccess cont, TCont fail)
    {
      BigInteger i = (BigInteger) operand1;
      BigInteger j = (BigInteger) operand2;
      return cont.call(i.divide(j), fail);
    }

    public Callable<Callable> applyN(Object[] operands, MapEnv env, TSuccess cont, TCont fail)
    {
      BigInteger r = (BigInteger) operands[0];
      for (int i = 1; i < operands.length; i++)
      {
        r = r.divide((BigInteger) operands[i]);
      }
      return cont.call(r, fail);
    }
  }

  public static final class IIMinus extends Procedure
  {
    public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(operand, fail);
    }

    public Callable<Callable> apply2(Object operand1, Object operand2, MapEnv env, TSuccess cont, TCont fail)
    {
      BigInteger i = (BigInteger) operand1;
      BigInteger j = (BigInteger) operand2;
      return cont.call(i.subtract(j), fail);
    }

    public Callable<Callable> applyN(Object[] operands, MapEnv env, TSuccess cont, TCont fail)
    {
      BigInteger r = (BigInteger) operands[0];
      for (int i = 1; i < operands.length; i++)
      {
        r = r.subtract((BigInteger) operands[i]);
      }
      return cont.call(r, fail);
    }
  }

  public static final class Not extends Procedure
  {
    public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(Boolean.FALSE.equals(operand) ? Boolean.TRUE : Boolean.FALSE, fail);
    }
  }

  public static final class Equals extends Procedure
  {
    public Callable<Callable> apply2(Object operand1, Object operand2, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(operand1.equals(operand2), fail);
    }

    public Callable<Callable> applyN(Object[] operands, MapEnv env, TSuccess cont, TCont fail)
    {
      Object r = operands[0];
      for (int i = 1; i < operands.length; i++)
      {
        if (!r.equals(operands[i]))
        {
          return cont.call(Boolean.FALSE, fail);
        }
      }
      return cont.call(Boolean.TRUE, fail);
    }
  }

  public static final class IZeroP extends Procedure
  {
    public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(operand.equals(IZERO), fail);
    }
  }

  public static final class IIZeroP extends Procedure
  {
    public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(operand.equals(BigInteger.ZERO), fail);
    }
  }

  public static final class Length extends Procedure
  {
    public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call((int) ((Lst) operand).length(), fail);
    }
  }

  public static final class ListRef extends Procedure
  {
    public Callable<Callable> apply2(Object operand1, Object operand2, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(((Lst) operand1).listRef((Integer) operand2), fail);
    }
  }

  public static final class Reverse extends Procedure
  {
    public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(((Lst) operand).reverse(), fail);
    }
  }

  public static final class NullP extends Procedure
  {
    public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(operand instanceof Null, fail);
    }
  }

  public static final class EqP extends Procedure
  {
    public Callable<Callable> apply2(Object operand1, Object operand2, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(Primitives.eq(operand1, operand2), fail);
    }
  }

  public static final class EqvP extends Procedure
  {
     public Callable<Callable> apply2(Object operand1, Object operand2, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(Primitives.eqv(operand1, operand2), fail);
    }
  }

  public static final class EqualP extends Procedure
  {
    public Callable<Callable> apply2(Object operand1, Object operand2, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(Primitives.equal(operand1, operand2), fail);
    }
  }

  public static final class LessThan extends Procedure
  {
    public Callable<Callable> apply2(Object operand1, Object operand2, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(((Comparable) operand1).compareTo(operand2) < 0, fail);
    }

    public Callable<Callable> applyN(Object[] operands, MapEnv env, TSuccess cont, TCont fail)
    {
      Comparable c = (Comparable) operands[0];
      for (int i = 1; i < operands.length; i++)
      {
        if (c.compareTo(operands[i]) > -1)
        {
          return cont.call(Boolean.FALSE, fail);
        }
      }
      return cont.call(Boolean.TRUE, fail);
    }
  }

  public static final class LessThanOrEquals extends Procedure
  {
    public Callable<Callable> apply2(Object operand1, Object operand2, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(((Comparable) operand1).compareTo(operand2) <= 0, fail);
    }

    public Callable<Callable> applyN(Object[] operands, MapEnv env, TSuccess cont, TCont fail)
    {
      Comparable c = (Comparable) operands[0];
      for (int i = 1; i < operands.length; i++)
      {
        if (c.compareTo(operands[i]) > 0)
        {
          return cont.call(Boolean.FALSE, fail);
        }
      }
      return cont.call(Boolean.TRUE, fail);
    }
  }

  public static final class GreaterThan extends Procedure
  {
    public Callable<Callable> apply2(Object operand1, Object operand2, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(((Comparable) operand1).compareTo(operand2) > 0, fail);
    }

    public Callable<Callable> applyN(Object[] operands, MapEnv env, TSuccess cont, TCont fail)
    {
      Comparable c = (Comparable) operands[0];
      for (int i = 1; i < operands.length; i++)
      {
        if (c.compareTo(operands[i]) < 1)
        {
          return cont.call(Boolean.FALSE, fail);
        }
      }
      return cont.call(Boolean.TRUE, fail);
    }
  }

  public static final class GreaterThanOrEquals extends Procedure
  {
    public Callable<Callable> apply2(Object operand1, Object operand2, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(((Comparable) operand1).compareTo(operand2) >= 0, fail);
    }

    public Callable<Callable> applyN(Object[] operands, MapEnv env, TSuccess cont, TCont fail)
    {
      Comparable c = (Comparable) operands[0];
      for (int i = 1; i < operands.length; i++)
      {
        if (c.compareTo(operands[i]) < 0)
        {
          return cont.call(Boolean.FALSE, fail);
        }
      }
      return cont.call(Boolean.TRUE, fail);
    }
  }

  public static final class Error extends Procedure
  {
    public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(Primitives.error(operand), fail);
    }
  }

  public static final class Cons extends Procedure
  {
    public Callable<Callable> apply2(Object operand1, Object operand2, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(Pair.cons(operand1, operand2), fail);
    }
  }

  public static final class List extends Procedure
  {
    public Callable<Callable> apply0(MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(new Null(), fail);
    }

    public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(Pair.cons(operand, new Null()), fail);
    }

    public Callable<Callable> apply2(Object operand1, Object operand2, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(Pair.cons(operand1, Pair.cons(operand2, new Null())), fail);
    }

    public Callable<Callable> apply3(Object operand1, Object operand2, Object operand3, MapEnv env, TSuccess cont,
        TCont fail)
    {
      return cont.call(Pair.cons(operand1, Pair.cons(operand2, Pair.cons(operand3, new Null()))), fail);
    }

    public Callable<Callable> applyN(Object[] operands, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(Lst.valueOf(operands), fail);
    }
  }

  public static final class ImproperList extends Procedure
  {
    public Callable<Callable> apply2(Object operand1, Object operand2, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(Pair.cons(operand1, operand2), fail);
    }

    public Callable<Callable> apply3(Object operand1, Object operand2, Object operand3, MapEnv env, TSuccess cont,
        TCont fail)
    {
      return cont.call(Pair.cons(operand1, Pair.cons(operand2, operand3)), fail);
    }

    public Callable<Callable> applyN(Object[] operands, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(Lst.valueOfImproper(operands), fail);
    }
  }

  public static final class Map extends Procedure
  {
    public Callable<Callable> apply2(Object operator, Object list, final MapEnv env, final TSuccess cont,
        final TCont fail)
    {
      if (list instanceof Null)
      {
        return cont.call(new Null(), fail);
      }
      final Procedure op = (Procedure) operator;
      final Pair lst = (Pair) list;
      return op.apply1(lst.car(), env, new TSuccess()
      {
        private java.util.List<Object> mapped = new ArrayList<Object>();
        private Lst l = (Lst) lst.cdr();

        public Callable<Callable> call(Object value, TCont failure)
        {
          mapped.add(value);
          if (l.isNull())
          {
            return cont.call(Lst.valueOf(mapped), fail);
          }
          else
          {
            Object car = l.car();
            l = (Lst) l.cdr();
            return op.apply1(car, env, this, fail);
          }
        }
      }, fail);
    }
    // TODO other apply2, ..., applyN
  }

  public static final class PMap extends Procedure
  {
    private ExecutorService executor;

    public PMap(ExecutorService executor)
    {
      super();
      this.executor = executor;
    }

    public Callable<Callable> apply2(Object operator, Object list, final MapEnv env, final TSuccess cont,
        final TCont fail)
    {
      final Object[] els = ((Lst) list).properToArray();
      final int l = els.length;
      final Object[] nels = new Object[l];
      final Procedure proc = (Procedure) operator;
      java.util.List<Callable<Object>> tasks = new ArrayList<Callable<Object>>();
      for (int i = 0; i < l; i++)
      {
        final int j = i;
        tasks.add(new Callable()
        {
          public Object call() throws Exception
          {
            TSuccess.trampoline(proc.apply1(els[j], env, new TSuccess()
            {
              public Callable<Callable> call(Object val, TCont fail)
              {
                nels[j] = val;
                return null;
              }
            }, fail));
            return null;
          }
        });
      }
      try
      {
        executor.invokeAll(tasks);
      }
      catch (Exception e)
      {
        throw new StremeException("error invoking parallel map tasks", e);
      }
      return cont.call(Lst.valueOf(nels), fail);
    }
    // TODO other apply2, ..., applyN
  }

  public static final class ForEach extends Procedure
  {
    public Callable<Callable> apply2(Object operator, Object list, final MapEnv env, final TSuccess cont, TCont fail)
    {
      if (list instanceof Null)
      {
        return cont.call(new Null(), fail);
      }
      final Procedure op = (Procedure) operator;
      final Pair lst = (Pair) list;
      return op.apply1(lst.car(), env, new TSuccess()
      {
        private Lst l = (Lst) lst.cdr();

        public Callable<Callable> call(Object value, TCont fail)
        {
          if (l.isNull())
          {
            return cont.call(Literal.UNSPECIFIED, fail);
          }
          else
          {
            Object car = l.car();
            l = (Lst) l.cdr();
            return op.apply1(car, env, this, fail);
          }
        }
      }, fail);
    }
    // TODO other apply2, ..., applyN
  }

  public static final class Append extends Procedure
  {
    public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(operand, fail);
    }

    public Callable<Callable> apply2(Object operand1, Object operand2, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(((Lst) operand1).append((Lst) operand2), fail);
    }

    public Callable<Callable> apply3(Object operand1, Object operand2, Object operand3, MapEnv env, TSuccess cont,
        TCont fail)
    {
      return cont.call(((Lst) operand1).append((Lst) operand2).append((Lst) operand3), fail);
    }
    
    public Callable<Callable> applyN(Object[] operands, MapEnv env, TSuccess cont,
        TCont fail)
    {
      java.util.List<Object> els = new ArrayList<Object>();
      for (Object operand : operands)
      {
        for (Object el : (Lst) operand)
        {
          els.add(el);
        }
      }
      return cont.call(Lst.valueOf(els), fail);
    }
  }

  public static final class Memq extends Procedure
  {
    public Callable<Callable> apply2(Object candidate, Object list, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(Primitives.memq(candidate, list), fail);
    }
  }

  public static final class Member extends Procedure
  {
    public Callable<Callable> apply2(Object candidate, Object list, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(Primitives.member(candidate, list), fail);
    }
  }

  public static final class Memv extends Procedure
  {
    public Callable<Callable> apply2(Object candidate, Object list, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(Primitives.memv(candidate, list), fail);
    }
  }


  public static final class Assoc extends Procedure // UNTESTED
  {
    public Callable<Callable> apply2(Object candidate, Object list, MapEnv env, TSuccess cont, TCont fail)
    {
      while (!(list instanceof Null))
      {
        Pair p = (Pair) list;
        Pair c = (Pair) p.car();
        if (Primitives.equal(candidate, c.car()))
        {
          return cont.call(c, fail);
        }
        list = p.cdr();
      }
      return cont.call(Boolean.FALSE, fail);
    }
  }

  public static final class Assq extends Procedure // UNTESTED
  {
    public Callable<Callable> apply2(Object candidate, Object list, MapEnv env, TSuccess cont, TCont fail)
    {
      while (!(list instanceof Null))
      {
        Pair p = (Pair) list;
        Pair c = (Pair) p.car();
        if (Primitives.eq(candidate, c.car()))
        {
          return cont.call(c, fail);
        }
        list = p.cdr();
      }
      return cont.call(Boolean.FALSE, fail);
    }
  }

  public static final class String2Symbol extends Procedure // UNTESTED
  {
    public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(new Sym((String) operand), fail);
    }
  }

  public static final class StringRef extends Procedure // UNTESTED
  {
    public Callable<Callable> apply2(Object string, Object index, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(((String) string).charAt((Integer) index), fail);
    }
  }

  public static final class Symbol2String extends Procedure // UNTESTED
  {
    public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(((Sym) operand).getName(), fail);
    }
  }

  public static final class Number2String extends Procedure // UNTESTED
  {
    public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(operand.toString(), fail);
    }
  }

  public static final class List2Vector extends Procedure // UNTESTED
  {
    public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(((Lst) operand).properToArray(), fail);
    }
  }

  public static final class Vector2List extends Procedure // UNTESTED
  {
    public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(Lst.valueOf((Object[]) operand), fail);
    }
  }

  public static final class ToF extends Procedure // UNTESTED
  {
    public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(((Number) operand).doubleValue(), fail);
    }
  }

  public static final class StringAppend extends Procedure // UNTESTED
  {
    public Callable<Callable> apply2(Object operand1, Object operand2, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(((String) operand1).concat((String) operand2), fail);
    }

    public Callable<Callable> apply3(Object operand1, Object operand2, Object operand3, MapEnv env, TSuccess cont,
        TCont fail)
    {
      return cont.call(((String) operand1).concat((String) operand2), fail);
    }
  }

  public static final class NumberP extends Procedure // UNTESTED
  {
    public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(operand instanceof Number || operand instanceof BigRational, fail);
    }
  }

  public static final class PrimitiveP extends Procedure // UNTESTED
  {
    public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(new Exception("TODO"), fail);
    }
  }

  public static final class ClassForName extends Procedure // UNTESTED
  {
    public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
    {
      try
      {
        String name = String.valueOf(operand);
        Class<?> clazz;
        if ("String".equals(name))
        {
          clazz = String.class;
        }
        else if ("Double".equals(name))
        {
          clazz = Double.class;
        }
        else if ("Math".equals(name))
        {
          clazz = Math.class;
        }
        else if ("System".equals(name))
        {
          clazz = System.class;
        }
        else if ("Runnable".equals(name))
        {
          clazz = Runnable.class;
        }
        else
        {
          clazz = Class.forName(name, true, classLoader);
        }
        return cont.call(clazz, fail);
      }
      catch (Exception e)
      {
        throw new StremeException("reflection error", e);
      }
    }
  }

  public static final class New extends Procedure // UNTESTED
  {
    public Callable<Callable> apply1(Object clazz, MapEnv env, TSuccess cont, TCont fail)
    {
      try
      {
        return cont.call(ConstructorUtils.invokeConstructor((Class) clazz, null), fail);
      }
      catch (Exception e)
      {
        throw new StremeException("reflection error", e);
      }
    }

    public Callable<Callable> apply2(Object clazz, Object arg, MapEnv env, TSuccess cont, TCont fail)
    {
      try
      {
        return cont.call(ConstructorUtils.invokeConstructor((Class) clazz, arg), fail);
      }
      catch (Exception e)
      {
        throw new StremeException("reflection error", e);
      }
    }

    public Callable<Callable> apply3(Object clazz, Object arg1, Object arg2, MapEnv env, TSuccess cont, TCont fail)
    {
      try
      {
        return cont.call(ConstructorUtils.invokeConstructor((Class) clazz, new Object[] { arg1, arg2}), fail);
      }
      catch (Exception e)
      {
        throw new StremeException("reflection error", e);
      }
    }

    public Callable<Callable> applyN(Object[] operands, MapEnv env, TSuccess cont, TCont fail)
    {
      Class<?> clazz = (Class) operands[0];
      Object[] args = Arrays.copyOfRange(operands, 1, operands.length);
      try
      {
        return cont.call(ConstructorUtils.invokeConstructor(clazz, args), fail);
      }
      catch (Exception e)
      {
        throw new StremeException("reflection error", e);
      }
    }
  }

  public static final class Invoke extends Procedure // UNTESTED
  {
    public Callable<Callable> apply2(Object instance, Object instanceMember, MapEnv env, TSuccess cont, TCont fail)
    {
      try
      {
        try
        {
          Object value = MethodUtils.invokeMethod(instance, String.valueOf(instanceMember), null);
          return cont.call(value, fail);
        }
        catch (NoSuchMethodException nsme)
        {
          try
          {
            Field field = instance.getClass().getField(String.valueOf(instanceMember));
            Object value = field.get(instance);
            return cont.call(value, fail);
          }
          catch (NoSuchFieldException nsfe)
          {
            throw new StremeException("no such method or field: " + instanceMember, nsfe);
          }
        }
      }
      catch (Exception e)
      {
        throw new StremeException("reflection error", e);
      }
    }

    public Callable<Callable> apply3(Object instance, Object instanceMember, Object arg, MapEnv env, TSuccess cont,
        TCont fail)
    {
      try
      {
        Object value = MethodUtils.invokeMethod(instance, String.valueOf(instanceMember), arg);
        return cont.call(value, fail);
      }
      catch (Exception e)
      {
        throw new StremeException("reflection error for invocation (" + instanceMember + " " + instance + " " + arg + ")", e);
      }
    }

    public Callable<Callable> applyN(Object[] operands, MapEnv env, TSuccess cont, TCont fail)
    {
      Object instance = operands[0];
      String instanceMember = String.valueOf(operands[1]);
      Object[] args = Arrays.copyOfRange(operands, 2, operands.length);
      try
      {
        Object value = MethodUtils.invokeMethod(instance, instanceMember, args);
        return cont.call(value, fail);
      }
      catch (Exception e)
      {
        throw new StremeException("reflection error", e);
      }
    }
  }

  public static final class InvokeStatic extends Procedure // UNTESTED
  {
    public Callable<Callable> apply2(Object clazz, Object staticMember, MapEnv env, TSuccess cont, TCont fail)
    {
      try
      {
        try
        {
          Object value = MethodUtils.invokeStaticMethod((Class) clazz, String.valueOf(staticMember), null);
          return cont.call(value, fail);
        }
        catch (NoSuchMethodException nsme)
        {
          try
          {
            Field field = ((Class) clazz).getField(String.valueOf(staticMember));
            Object value = field.get(null);
            return cont.call(value, fail);
          }
          catch (NoSuchFieldException nsfe)
          {
            throw new StremeException("no such static method or field: " + staticMember, nsfe);
          }
        }
      }
      catch (Exception e)
      {
        throw new StremeException("reflection error", e);
      }
    }

    public Callable<Callable> apply3(Object clazz, Object staticMember, Object arg, MapEnv env, TSuccess cont,
        TCont fail)
    {
      try
      {
        Object value = MethodUtils.invokeStaticMethod((Class) clazz, String.valueOf(staticMember), arg);
        return cont.call(value, fail);
      }
      catch (Exception e)
      {
        throw new StremeException("reflection error", e);
      }
    }

    public Callable<Callable> applyN(Object[] operands, MapEnv env, TSuccess cont, TCont fail)
    {
      Class<?> clazz = (Class) operands[0];
      String staticMember = String.valueOf(operands[1]);
      Object[] args = Arrays.copyOfRange(operands, 2, operands.length);
      try
      {
        Object value = MethodUtils.invokeStaticMethod(clazz, staticMember, args);
        return cont.call(value, fail);
      }
      catch (Exception e)
      {
        throw new StremeException("reflection error", e);
      }
    }
  }
  
  public static final class Unbound2False extends Procedure // UNTESTED
  {
    public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(operand == null ? new Boolean(false) : operand, fail);
    }
  }

  public static final class IterableP extends Procedure // UNTESTED
  {
    public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(operand instanceof Iterable, fail);
    }
  }

  public static final class Proxy extends Procedure // UNTESTED
  {
    private NdStremeContext context;

    public Proxy(NdStremeContext context)
    {
      super();
      this.context = context;
    }

    public Callable<Callable> apply2(Object interfaceNames, final Object bindings, final MapEnv env,
        final TSuccess cont, final TCont fail)
    {
      final Class[] interfaces = ((Lst) interfaceNames).properToArray(Class.class);
      final Lst handlerEnv = (Lst) bindings;
      Object proxy = java.lang.reflect.Proxy.newProxyInstance(context.getClass().getClassLoader(), interfaces,
          new InvocationHandler()
          {
            public Object invoke(Object proxy, Method method, Object[] args) throws Throwable
            {
              String name = method.getName();
              Object assoc = ((Lst) bindings).assoc(new Sym(name));
              if (Boolean.FALSE.equals(assoc))
              {
                throw new StremeException("cannot invoke " + name + " on proxy " + proxy);
              }
              Procedure proc = (Procedure) ((Pair) assoc).cdr();
              final Object[] result = new Object[1];
              TSuccess rc = new TSuccess()
              {
                public Callable<Callable> call(Object value, TCont failure)
                {
                  result[0] = value;
                  return null;
                }
              };
              switch (args.length)
              {
                case 0:
                  TSuccess.trampoline(proc.apply0(env, rc, fail));
                  break;
                case 1:
                  TSuccess.trampoline(proc.apply1(args[0], env, rc, fail));
                  break;
                case 2:
                  TSuccess.trampoline(proc.apply2(args[0], args[1], env, rc, fail));
                  break;
                case 3:
                  TSuccess.trampoline(proc.apply3(args[0], args[1], args[2], env, rc, fail));
                  break;
                default:
                  TSuccess.trampoline(proc.applyN(args, env, rc, fail));
                  break;
              }
              return result[0];
            }
          });
      return cont.call(proxy, fail);
    }
  }

  public static final class GetField extends Procedure // UNTESTED
  {
    public Callable<Callable> apply2(Object instance, Object instanceMember, MapEnv env, TSuccess cont, TCont fail)
    {
      try
      {
        Field field = instance.getClass().getField(String.valueOf(instanceMember));
        Object value = field.get(instance);
        return cont.call(value, fail);
      }
      catch (Exception e)
      {
        throw new StremeException("reflection error", e);
      }
    }
  }

  public static final class GetStaticField extends Procedure // UNTESTED
  {
    public Callable<Callable> apply2(Object clazz, Object staticMember, MapEnv env, TSuccess cont, TCont fail)
    {
      try
      {
        Field field = ((Class) clazz).getField(String.valueOf(staticMember));
        Object value = field.get(null);
        return cont.call(value, fail);
      }
      catch (Exception e)
      {
        throw new StremeException("reflection error", e);
      }
    }
  }

  // private static final Class<?> getClazz(Object className) throws ClassNotFoundException
  // {
  // String name = String.valueOf(className);
  // if ("Class".equals(name))
  // {
  // return Class.class;
  // }
  // if ("String".equals(name))
  // {
  // return String.class;
  // }
  // if ("System".equals(name))
  // {
  // return System.class;
  // }
  // return Class.forName(name);
  // }
  public static final class SymbolP extends Procedure// UNTESTED
  {
    public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(operand instanceof Sym, fail);
    }
  }

  public static final class PairP extends Procedure// UNTESTED
  {
    public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(operand instanceof Pair, fail);
    }
  }

  public static final class ListP extends Procedure// UNTESTED
  {
    public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
    {
      while (operand instanceof Lst)
      {
        if (operand instanceof Null)
        {
          return cont.call(Boolean.TRUE, fail);
        }
        operand = ((Lst) operand).cdr();
      }
      return cont.call(Boolean.FALSE, fail);
    }
  }

  public static final class Car extends Procedure
  {
    public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(((Lst) operand).car(), fail);
    }
  }

  public static final class Caadr extends Procedure // UNTESTED
  {
    public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(((Lst) operand).caadr(), fail);
    }
  }

  public static final class Cadadr extends Procedure // UNTESTED
  {
    public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(((Lst) operand).cadadr(), fail);
    }
  }

  public static final class Cadddr extends Procedure // UNTESTED
  {
    public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(((Lst) operand).cadddr(), fail);
    }
  }

  public static final class Caddr extends Procedure // UNTESTED
  {
    public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(((Lst) operand).caddr(), fail);
    }
  }

  public static final class Cadr extends Procedure // UNTESTED
  {
    public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(((Lst) operand).cadr(), fail);
    }
  }

  public static final class Cdar extends Procedure // UNTESTED
  {
    public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(((Lst) operand).cdar(), fail);
    }
  }

  public static final class Caar extends Procedure // UNTESTED
  {
    public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(((Lst) operand).caar(), fail);
    }
  }

  public static final class Cddr extends Procedure // UNTESTED
  {
    public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(((Lst) operand).cddr(), fail);
    }
  }

  public static final class Cdddr extends Procedure // UNTESTED
  {
    public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(((Lst) operand).cdddr(), fail);
    }
  }

  public static final class Cdr extends Procedure
  {
    public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(((Lst) operand).cdr(), fail);
    }
  }

  public static final class Display extends Procedure
  {
    public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
    {
      System.out.print(operand);
      return cont.call(Void.TYPE, fail);
    }
  }

  public static final class Eval extends Procedure // UNTESTED
  {
    private NdStremeContext context;

    public Eval(NdStremeContext context)
    {
      super();
      this.context = context;
    }

    /**
     * Eval in current environment: not R5RS
     */
    public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
    {
      return context.evaluateData(operand, env, cont, fail);
    }

    public Callable<Callable> apply2(Object operand, Object evalEnv, MapEnv env, TSuccess cont, TCont fail)
    {
      return context.evaluateData(operand, (MapEnv) evalEnv, cont, fail);
    }
  }

  public static final class Apply extends Procedure // UNTESTED
  {
    public Callable<Callable> apply2(Object operator, Object operands, MapEnv env, TSuccess cont, TCont fail)
    {
      Procedure proc = (Procedure) operator;
      Object[] ops = ((Lst) operands).properToArray();
      switch (ops.length)
      {
        case 0:
          return proc.apply0(env, cont, fail);
        case 1:
          return proc.apply1(ops[0], env, cont, fail);
        case 2:
          return proc.apply2(ops[0], ops[1], env, cont, fail);
        case 3:
          return proc.apply3(ops[0], ops[1], ops[2], env, cont, fail);
        default:
          return proc.applyN(ops, env, cont, fail);
      }
    }
  }

  public static final class Newline extends Procedure
  {
    public Callable<Callable> apply0(MapEnv env, TSuccess cont, TCont fail)
    {
      System.out.println();
      return cont.call(Void.TYPE, fail);
    }
  }

  public static final class InteractionEnvironment extends Procedure
  {
    private NdStremeContext context;

    public InteractionEnvironment(NdStremeContext context)
    {
      super();
      this.context = context;
    }

    public Callable<Callable> apply0(MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(context.globalEnv(), fail);
    }
  }

  public static final class CurrentEnvironment extends Procedure
  {
    public Callable<Callable> apply0(MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(env, fail);
    }
  }

  public static final class Read extends Procedure
  {
    private NdStremeContext context;

    public Read(NdStremeContext context)
    {
      super();
      this.context = context;
    }

    public Callable<Callable> apply0(MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(context.read(new InputStreamReader(System.in)), fail);
    }
  }

  public static final class StringLessThanP extends Procedure
  {
    public Callable<Callable> apply2(Object operand1, Object operand2, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(((String) operand1).compareTo((String) operand2) < 0, fail);
    }
  }

  public static final class Touch extends Procedure // UNTESTED
  {
    public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
    {
      if (operand instanceof Future<?>)
      {
        Future<?> future = (Future<?>) operand;
        try
        {
          // long start = System.currentTimeMillis();
          Object value = future.get();
          // long end = System.currentTimeMillis();
          // System.out.println("waited " + (end - start));
          return cont.call(value, fail);
        }
        catch (Exception e)
        {
          throw new StremeException("error touching " + operand, e);
        }
      }
      else
      {
        return cont.call(operand, fail);
      }
    }
    // public Callable<Callable> apply2(Object operand, Object onResolveObject, final MapEnv env, TSuccess cont, TCont
    // fail)
    // {
    // final Procedure onResolve = (Procedure) onResolveObject;
    // if (operand instanceof Future<?>)
    // {
    // Future<?> future = (Future<?>) operand;
    // try
    // {
    // future.addOnResolve(new RunnableCont(context.executor(), "futureOnResolve")
    // {
    // protected void execute(Object value)
    // {
    // onResolve.apply1(context, value, env, new DefaulTSuccess("empty") // FIXME
    // {
    // public void call(Object val)
    // {
    // }
    // });
    // }
    // });
    // return cont.call(future);
    // }
    // catch (Exception e)
    // {
    // throw new StremeException("error touching " + operand, e);
    // }
    // }
    // else
    // {
    // onResolve.apply1(context, operand, env, cont);
    // }
    // }
  }

  public static final class Load extends Procedure // UNTESTED
  {
    private NdStremeContext context;

    public Load(NdStremeContext context)
    {
      super();
      this.context = context;
    }

    public Callable<Callable> apply1(Object operand, MapEnv env, final TSuccess cont, TCont fail)
    {
      final Reader reader;
      try
      {
        reader = new InputStreamReader(getClass().getResourceAsStream(operand.toString()));
      }
      catch (Exception e)
      {
        throw new StremeException("error loading " + operand, e);
      }
      return load(reader, cont, fail);
    }

    private Callable<Callable> load(final Reader reader, final TSuccess cont, TCont fail)
    {
      Object parsed = context.read(reader);
      if (parsed != null)
      {
        return context.evaluateData(parsed, context.globalEnv(), new TSuccess()
        {
          public Callable<Callable> call(Object value, TCont fail)
          {
            Object parsed = context.read(reader);
            if (parsed == null)
            {
              try
              {
                reader.close();
              }
              catch (IOException e)
              {
                throw new StremeException("load", e);
              }
              return cont.call(value, fail);
            }
            else
            {
              return context.evaluateData(parsed, context.globalEnv(), this, fail);
            }
          }
        }, fail);
      }
      else
      {
        return cont.call(Void.TYPE, fail);
      }
    }
  }

  public static final class VectorRef extends Procedure
  {
    public Callable<Callable> apply2(Object vector, Object index, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(((Object[]) vector)[(Integer) index], fail);
    }
  }

  public static final class MakeVector extends Procedure
  {
    public Callable<Callable> apply1(Object length, MapEnv env, TSuccess cont, TCont fail)
    {
      int l = (Integer) length;
      Object[] v = new Object[l];
      Arrays.fill(v, BigInteger.ZERO);
      return cont.call(v, fail);
    }

    public Callable<Callable> apply2(Object length, Object init, MapEnv env, TSuccess cont, TCont fail)
    {
      int l = (Integer) length;
      Object[] v = new Object[l];
      Arrays.fill(v, init);
      return cont.call(v, fail);
    }
  }

  public static final class Vector extends Procedure
  {
    public Callable<Callable> apply0(Object operand, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(new Object[0], fail);
    }

    public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(new Object[] { operand}, fail);
    }

    public Callable<Callable> apply2(Object operand1, Object operand2, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(new Object[] { operand1, operand2}, fail);
    }

    public Callable<Callable> apply3(Object operand1, Object operand2, Object operand3, MapEnv env, TSuccess cont,
        TCont fail)
    {
      return cont.call(new Object[] { operand1, operand2, operand3}, fail);
    }

    public Callable<Callable> applyN(Object[] operands, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(operands, fail);
    }
  }

  public static final class IModulo extends Procedure
  {
    public Callable<Callable> apply2(Object operand1, Object operand2, MapEnv env, TSuccess cont, TCont fail)
    {
      int i = (Integer) operand1;
      int j = (Integer) operand2;
      int rem = i % j;
      if (j < 0 && rem > 0)
      {
        return cont.call(rem + j, fail);
      }
      else if (j > 0 && rem < 0)
      {
        return cont.call(rem + j, fail);
      }
      else
      {
        return cont.call(rem, fail);
      }
    }
  }

  public static final class IQuotient extends Procedure
  {
    public Callable<Callable> apply2(Object operand1, Object operand2, MapEnv env, TSuccess cont, TCont fail)
    {
      int i = (Integer) operand1;
      int j = (Integer) operand2;
      int r = i / j;
      return cont.call(r, fail);
    }
  }

  public static final class NegativeP extends Procedure
  {
    public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
    {
      if (operand instanceof Integer)
      {
        return cont.call(((Integer) operand) < 0, fail);
      }
      if (operand instanceof BigInteger)
      {
        return cont.call(((BigInteger) operand).compareTo(BigInteger.ZERO) < 0, fail);
      }
      throw new IllegalArgumentException("(negative? " + operand + ")");
    }
  }

  public static final class IOddP extends Procedure
  {
    public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(((Integer) operand) % 2 != 0, fail);
    }
  }

  public static final class IIOddP extends Procedure
  {
    public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(Primitives.oddp(operand), fail);
    }
  }

  public static final class IEvenP extends Procedure
  {
    public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(((Integer) operand) % 2 == 0, fail);
    }
  }

  public static final class IIEvenP extends Procedure
  {
    public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
    {
      return cont.call(Primitives.evenp(operand), fail);
    }
  }

  public static final class Unify extends Procedure
  {
    private final DataUnifier unifier = new DataUnifier();

    public Callable<Callable> apply2(Object operand1, Object operand2, MapEnv env, TSuccess cont, TCont fail)
    {
      java.util.Map<Sym, Object> result = unifier.unify(operand1, operand2);
      return cont.call(result == null ? new Boolean(false) : Lst.assocValueOf(result), fail);
    }
  }

  public static final class Sleep extends Procedure
  {
    public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
    {
      try
      {
        Thread.sleep((Integer) operand);
      }
      catch (InterruptedException e)
      {
        throw new StremeException("sleep interrupted", e);
      }
      return cont.call(Void.TYPE, fail);
    }
  }

  public static final class FixedRandomList extends Procedure
  {
    public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
    {
      Random random = new Random(12345);
      Lst l = new Null();
      int m = (Integer) operand;
      for (int i = 0; i < m; i++)
      {
        l = Pair.cons(random.nextInt(), l);
      }
      return cont.call(l, fail);
    }
  }

  // public static final class Serializer_ extends Procedure
  // {
  // public Callable<Callable> apply0(MapEnv env, TSuccess cont, TCont fail)
  // {
  // return cont.call(new Serializer());
  // }
  // }
  //
  // public static final class Serial extends Procedure
  // {
  // public Callable<Callable> apply2(Object operand1, Object operand2, final MapEnv env, final TSuccess cont, TCont
  // fail)
  // {
  // Serializer s = (Serializer) operand1;
  // final Procedure p = (Procedure) operand2;
  // final FutureCont fc = new FutureCont();
  // return cont.call(fc);
  // s.execute(new DefaulTSuccess("serialize")
  // {
  // public void call(Object unlocker)
  // {
  // p.apply1(context, unlocker, env, fc);
  // }
  // });
  // }
  // }
  //
  // public static final class Yield extends Procedure
  // {
  // public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
  // {
  // ((Serializer.Unlocker) operand).yield();
  // return cont.call(Void.TYPE, fail);
  // }
  // }
  private NdStremePrimitives()
  {
    super();
  }
}
