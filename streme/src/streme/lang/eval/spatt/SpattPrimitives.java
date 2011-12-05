package streme.lang.eval.spatt;

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
import streme.lang.data.Data;
import streme.lang.data.DataUnifier;
import streme.lang.data.Lst;
import streme.lang.data.Null;
import streme.lang.data.Pair;
import streme.lang.data.Sym;
import streme.lang.eval.ConstructorUtils;
import streme.lang.eval.LstEnv;
import streme.lang.eval.MethodUtils;
import streme.lang.eval.Primitives;
import streme.lang.eval.tanfe.StremeContext;

public class SpattPrimitives
{
  public static void loadPrimitives(LstEnv env, StremeContext context)
  {
    /* testing primitives */
    env.add(new Sym("fixed-random-list"), new FixedRandomList());
    /* Java interop */
    env.add(new Sym("new"), new New());
    env.add(new Sym("class-for-name"), new ClassForName());
    env.add(new Sym("invoke"), new Invoke());
    env.add(new Sym("invoke-static"), new InvokeStatic());
    env.add(new Sym("proxy"), new Proxy(context));
    /* Streme primitives */
    env.add(new Sym("improper-list"), new ImproperList());
    env.add(new Sym("primitive?"), new PrimitiveP());
    env.add(new Sym("load"), new Load(context));
    env.add(new Sym("touch"), new Touch());
    env.add(new Sym("current-environment"), new CurrentEnvironment());
    env.add(new Sym("unify"), new Unify());
    env.add(new Sym("sleep"), new Sleep());
    env.add(new Sym("error"), new Error());
    // env.add(new Sym("serializer"), new Serializer_());
    // env.add(new Sym("serial"), new Serial());
    // env.add(new Sym("yield"), new Yield());
    
    /* finger tree primitives */
//    env.add(new Sym("tree"), new Tree(context));
//    env.add(new Sym("equip"), new Equip());
//    env.add(new Sym("list->tree"), new List2Tree());
//    env.add(new Sym("split"), new Split());
//    env.add(new Sym("tree?"), new TreeP());
//    env.add(new Sym("empty?"), new EmptyP());
//    env.add(new Sym("singleton?"), new SingletonP());
//    env.add(new Sym("deep?"), new DeepP());
//    env.add(new Sym("left"), new Left());
//    env.add(new Sym("right"), new Right());
//    env.add(new Sym("lefttail"), new LeftTail());
//    env.add(new Sym("righttail"), new RightTail());
//    env.add(new Sym("addleft"), new AddLeft());
//    env.add(new Sym("addright"), new AddRight());
//    env.add(new Sym("conc"), new Conc());
//    env.add(new Sym("cached"), new Cached());
//    env.add(new Sym("tmap"), new TMap(context));
//    env.add(new Sym("tpsum"), new TPSum());
//    env.add(new Sym("tppsum"), new TPPSum(context.executor()));
//    env.add(new Sym("lpsum"), new LPSum());
//    env.add(new Sym("lpsum-array"), new LPSumArray());
    
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
    env.add(new Sym("string<=?"), new StringLessThanEqualP());
    env.add(new Sym("string>?"), new StringGreaterThanP());
    env.add(new Sym("string>=?"), new StringGreaterThanEqualP());
    env.add(new Sym("string-append"), new StringAppend());
    env.add(new Sym("string-length"), new StringLength());
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
    env.add(new Sym("memv"), new Memv());
    env.add(new Sym("memq"), new Memq());
    env.add(new Sym("member"), new Member());
    env.add(new Sym("append"), new Append());
    env.add(new Sym("map||"), new PMap(context.executor()));
    env.add(new Sym("map"), new Map());
    env.add(new Sym("for-each"), new ForEach());
    env.add(new Sym("list"), new List());
    env.add(new Sym("set-car!"), new SetCarB());
    env.add(new Sym("set-cdr!"), new SetCdrB());
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
    env.add(new Sym("vector-set!"), new VectorSetB());
    env.add(new Sym("vector-ref"), new VectorRef());
    env.add(new Sym("vector-length"), new VectorLength());
    env.add(new Sym("make-vector"), new MakeVector());
    env.add(new Sym("vector"), new Vector());
    env.add(new Sym("vector?"), new VectorP());
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
    env.add(new Sym("max"), new Max());
    // env.add(new Sym("F/"), new FFDivide());
    // env.add(new Sym("F-"), new FFMinus());
    // env.add(new Sym("F+"), new FFPlus());
    // env.add(new Sym("F*"), new FFMultiply());
  }

  public static final Integer IZERO = Integer.valueOf(0);

  public static final class Error extends Procedure
  {
    public Callable<Callable> apply1(Object operand, LstEnv env, TCont cont)
    {
      return cont.call(Primitives.error(operand));
    }
  }
  
  public static final class CallWithCurrentContinuation extends Procedure
  {
    public Callable<Callable> apply1(Object calledWithCc, final LstEnv env, final TCont cont)
    {
      Procedure calledWithCurrentContinuation = (Procedure) calledWithCc;
      return calledWithCurrentContinuation.apply1(new Procedure()
      {
        public Callable<Callable> apply1(Object operand, LstEnv e, TCont k)
        {
          return cont.call(operand);
        }
      }, env, cont);
    }
  }

  public static final class IIPlus extends Procedure
  {
    public Callable<Callable> apply1(Object operand, LstEnv env, TCont cont)
    {
      return cont.call(operand);
    }

    public Callable<Callable> apply2(Object operand1, Object operand2, LstEnv env, TCont cont)
    {
      BigInteger i = (BigInteger) operand1;
      BigInteger j = (BigInteger) operand2;
      return cont.call(i.add(j));
    }

    public Callable<Callable> applyN(Object[] operands, LstEnv env, TCont cont)
    {
      BigInteger r = (BigInteger) operands[0];
      for (int i = 1; i < operands.length; i++)
      {
        r = r.add((BigInteger) operands[i]);
      }
      return cont.call(r);
    }
  }

  public static final class IPlus extends Procedure
  {
    public Callable<Callable> apply1(Object operand, LstEnv env, TCont cont)
    {
      return cont.call(operand);
    }

    public Callable<Callable> apply2(Object operand1, Object operand2, LstEnv env, TCont cont)
    {
      int i = (Integer) operand1;
      int j = (Integer) operand2;
      return cont.call(i + j);
    }

    public Callable<Callable> apply3(Object operand1, Object operand2, Object operand3, LstEnv env, TCont cont)
    {
      int i = (Integer) operand1;
      int j = (Integer) operand2;
      int k = (Integer) operand3;
      return cont.call(i + j + k);
    }

    public Callable<Callable> applyN(Object[] operands, LstEnv env, TCont cont)
    {
      int r = (Integer) operands[0];
      for (int i = 1; i < operands.length; i++)
      {
        r += (Integer) operands[i];
      }
      return cont.call(r);
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

    public Callable<Callable> apply1(Object operand, LstEnv env, TCont cont)
    {
      return cont.call(operand);
    }

    public Callable<Callable> apply2(Object operand1, Object operand2, LstEnv env, TCont cont)
    {
      return cont.call(plus(operand1, operand2));
    }

    public Callable<Callable> apply3(Object operand1, Object operand2, Object operand3, LstEnv env, TCont cont)
    {
      int i = (Integer) operand1;
      int j = (Integer) operand2;
      int k = (Integer) operand3;
      return cont.call(i + j + k);
    }

    public Callable<Callable> applyN(Object[] operands, LstEnv env, TCont cont)
    {
      int r = (Integer) operands[0];
      for (int i = 1; i < operands.length; i++)
      {
        r += (Integer) operands[i];
      }
      return cont.call(r);
    }
  }

  public static final class FPlus extends Procedure
  {
    public Callable<Callable> apply1(Object operand, LstEnv env, TCont cont)
    {
      return cont.call(operand);
    }

    public Callable<Callable> apply2(Object operand1, Object operand2, LstEnv env, TCont cont)
    {
      double i = (Double) operand1;
      double j = (Double) operand2;
      return cont.call(i + j);
    }

    public Callable<Callable> apply3(Object operand1, Object operand2, Object operand3, LstEnv env, TCont cont)
    {
      double i = (Double) operand1;
      double j = (Double) operand2;
      double k = (Double) operand3;
      return cont.call(i + j + k);
    }

    public Callable<Callable> applyN(Object[] operands, LstEnv env, TCont cont)
    {
      double r = (Double) operands[0];
      for (int i = 1; i < operands.length; i++)
      {
        r += (Double) operands[i];
      }
      return cont.call(r);
    }
  }

  public static final class FMinus extends Procedure
  {
    public Callable<Callable> apply1(Object operand, LstEnv env, TCont cont)
    {
      return cont.call(operand);
    }

    public Callable<Callable> apply2(Object operand1, Object operand2, LstEnv env, TCont cont)
    {
      double i = (Double) operand1;
      double j = (Double) operand2;
      return cont.call(i - j);
    }

    public Callable<Callable> apply3(Object operand1, Object operand2, Object operand3, LstEnv env, TCont cont)
    {
      double i = (Double) operand1;
      double j = (Double) operand2;
      double k = (Double) operand3;
      return cont.call(i - j - k);
    }

    public Callable<Callable> applyN(Object[] operands, LstEnv env, TCont cont)
    {
      double r = (Double) operands[0];
      for (int i = 1; i < operands.length; i++)
      {
        r -= (Double) operands[i];
      }
      return cont.call(r);
    }
  }

  public static final class FMultiply extends Procedure
  {
    public Callable<Callable> apply1(Object operand, LstEnv env, TCont cont)
    {
      return cont.call(operand);
    }

    public Callable<Callable> apply2(Object operand1, Object operand2, LstEnv env, TCont cont)
    {
      double i = (Double) operand1;
      double j = (Double) operand2;
      return cont.call(i * j);
    }

    public Callable<Callable> apply3(Object operand1, Object operand2, Object operand3, LstEnv env, TCont cont)
    {
      double i = (Double) operand1;
      double j = (Double) operand2;
      double k = (Double) operand3;
      return cont.call(i * j * k);
    }

    public Callable<Callable> applyN(Object[] operands, LstEnv env, TCont cont)
    {
      double r = (Double) operands[0];
      for (int i = 1; i < operands.length; i++)
      {
        r *= (Double) operands[i];
      }
      return cont.call(r);
    }
  }

  public static final class FDivide extends Procedure
  {
    public Callable<Callable> apply1(Object operand, LstEnv env, TCont cont)
    {
      return cont.call(operand);
    }

    public Callable<Callable> apply2(Object operand1, Object operand2, LstEnv env, TCont cont)
    {
      double i = (Double) operand1;
      double j = (Double) operand2;
      return cont.call(i / j);
    }

    public Callable<Callable> apply3(Object operand1, Object operand2, Object operand3, LstEnv env, TCont cont)
    {
      double i = (Double) operand1;
      double j = (Double) operand2;
      double k = (Double) operand3;
      return cont.call(i / j / k);
    }

    public Callable<Callable> applyN(Object[] operands, LstEnv env, TCont cont)
    {
      double r = (Double) operands[0];
      for (int i = 1; i < operands.length; i++)
      {
        r /= (Double) operands[i];
      }
      return cont.call(r);
    }
  }

  public static final class Minus extends Procedure
  {
    public Callable<Callable> apply1(Object operand, LstEnv env, TCont cont)
    {
      return cont.call(operand);
    }

    public Callable<Callable> apply2(Object operand1, Object operand2, LstEnv env, TCont cont)
    {
      return cont.call(minus(operand1, operand2));
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

    public Callable<Callable> apply3(Object operand1, Object operand2, Object operand3, LstEnv env, TCont cont)
    {
      int i = (Integer) operand1;
      int j = (Integer) operand2;
      int k = (Integer) operand3;
      return cont.call(i - j - k);
    }

    public Callable<Callable> applyN(Object[] operands, LstEnv env, TCont cont)
    {
      int r = (Integer) operands[0];
      for (int i = 1; i < operands.length; i++)
      {
        r -= (Integer) operands[i];
      }
      return cont.call(r);
    }
  }

  public static final class IMinus extends Procedure
  {
    public Callable<Callable> apply1(Object operand, LstEnv env, TCont cont)
    {
      return cont.call(operand);
    }

    public Callable<Callable> apply2(Object operand1, Object operand2, LstEnv env, TCont cont)
    {
      int i = (Integer) operand1;
      int j = (Integer) operand2;
      return cont.call(i - j);
    }

    public Callable<Callable> apply3(Object operand1, Object operand2, Object operand3, LstEnv env, TCont cont)
    {
      int i = (Integer) operand1;
      int j = (Integer) operand2;
      int k = (Integer) operand3;
      return cont.call(i - j - k);
    }

    public Callable<Callable> applyN(Object[] operands, LstEnv env, TCont cont)
    {
      int r = (Integer) operands[0];
      for (int i = 1; i < operands.length; i++)
      {
        r -= (Integer) operands[i];
      }
      return cont.call(r);
    }
  }

  public static final class IMultiply extends Procedure
  {
    public Callable<Callable> apply1(Object operand, LstEnv env, TCont cont)
    {
      return cont.call(operand);
    }

    public Callable<Callable> apply2(Object operand1, Object operand2, LstEnv env, TCont cont)
    {
      int i = (Integer) operand1;
      int j = (Integer) operand2;
      return cont.call(i * j);
    }

    public Callable<Callable> apply3(Object operand1, Object operand2, Object operand3, LstEnv env, TCont cont)
    {
      int i = (Integer) operand1;
      int j = (Integer) operand2;
      int k = (Integer) operand3;
      return cont.call(i * j * k);
    }

    public Callable<Callable> applyN(Object[] operands, LstEnv env, TCont cont)
    {
      int r = (Integer) operands[0];
      for (int i = 1; i < operands.length; i++)
      {
        r *= (Integer) operands[i];
      }
      return cont.call(r);
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

    public Callable<Callable> apply1(Object operand, LstEnv env, TCont cont)
    {
      return cont.call(operand);
    }

    public Callable<Callable> apply2(Object operand1, Object operand2, LstEnv env, TCont cont)
    {
      return cont.call(multiply(operand1, operand2));
    }

    public Callable<Callable> apply3(Object operand1, Object operand2, Object operand3, LstEnv env, TCont cont)
    {
      int i = (Integer) operand1;
      int j = (Integer) operand2;
      int k = (Integer) operand3;
      return cont.call(i * j * k);
    }

    public Callable<Callable> applyN(Object[] operands, LstEnv env, TCont cont)
    {
      int r = (Integer) operands[0];
      for (int i = 1; i < operands.length; i++)
      {
        r *= (Integer) operands[i];
      }
      return cont.call(r);
    }
  }

  public static final class IDivide extends Procedure
  {
    public Callable<Callable> apply1(Object operand, LstEnv env, TCont cont)
    {
      return cont.call(operand);
    }

    public Callable<Callable> apply2(Object operand1, Object operand2, LstEnv env, TCont cont)
    {
      int i = (Integer) operand1;
      int j = (Integer) operand2;
      return cont.call(i / j);
    }

    public Callable<Callable> apply3(Object operand1, Object operand2, Object operand3, LstEnv env, TCont cont)
    {
      int i = (Integer) operand1;
      int j = (Integer) operand2;
      int k = (Integer) operand3;
      return cont.call(i / j / k);
    }

    public Callable<Callable> applyN(Object[] operands, LstEnv env, TCont cont)
    {
      int r = (Integer) operands[0];
      for (int i = 1; i < operands.length; i++)
      {
        r /= (Integer) operands[i];
      }
      return cont.call(r);
    }
  }

  public static final class Sin extends Procedure
  {
    public Callable<Callable> apply1(Object operand, LstEnv env, TCont cont)
    {
      return cont.call(Math.sin((Double) operand));
    }
  }

  public static final class Cos extends Procedure
  {
    public Callable<Callable> apply1(Object operand, LstEnv env, TCont cont)
    {
      return cont.call(Math.cos((Double) operand));
    }
  }

  public static final class Atan extends Procedure
  {
    public Callable<Callable> apply1(Object operand, LstEnv env, TCont cont)
    {
      return cont.call(Math.atan((Double) operand));
    }
  }

  public static final class Sqrt extends Procedure
  {
    public Callable<Callable> apply1(Object operand, LstEnv env, TCont cont)
    {
      return cont.call(Math.sqrt((Double) operand));
    }
  }

  public static final class Abs extends Procedure
  {
    public Callable<Callable> apply1(Object operand, LstEnv env, TCont cont)
    {
      return cont.call(Math.abs((Integer) operand));
    }
  }

  public static final class Max extends Procedure
  {
    public Callable<Callable> apply2(Object operand1, Object operand2, LstEnv env, TCont cont)
    {
      return cont.call(Primitives.max(operand1, operand2));
    }
  }

  public static final class IRemainder extends Procedure
  {
    public Callable<Callable> apply2(Object operand1, Object operand2, LstEnv env, TCont cont)
    {
      int i = (Integer) operand1;
      int j = (Integer) operand2;
      return cont.call(i % j);
    }
  }

  public static final class IIMultiply extends Procedure
  {
    public Callable<Callable> apply1(Object operand, LstEnv env, TCont cont)
    {
      return cont.call(operand);
    }

    public Callable<Callable> apply2(Object operand1, Object operand2, LstEnv env, TCont cont)
    {
      BigInteger i = (BigInteger) operand1;
      BigInteger j = (BigInteger) operand2;
      return cont.call(i.multiply(j));
    }

    public Callable<Callable> applyN(Object[] operands, LstEnv env, TCont cont)
    {
      BigInteger r = (BigInteger) operands[0];
      for (int i = 1; i < operands.length; i++)
      {
        r = r.multiply((BigInteger) operands[i]);
      }
      return cont.call(r);
    }
  }

  public static final class IIDivide extends Procedure
  {
    public Callable<Callable> apply1(Object operand, LstEnv env, TCont cont)
    {
      return cont.call(operand);
    }

    public Callable<Callable> apply2(Object operand1, Object operand2, LstEnv env, TCont cont)
    {
      BigInteger i = (BigInteger) operand1;
      BigInteger j = (BigInteger) operand2;
      return cont.call(i.divide(j));
    }

    public Callable<Callable> applyN(Object[] operands, LstEnv env, TCont cont)
    {
      BigInteger r = (BigInteger) operands[0];
      for (int i = 1; i < operands.length; i++)
      {
        r = r.divide((BigInteger) operands[i]);
      }
      return cont.call(r);
    }
  }

  public static final class IIMinus extends Procedure
  {
    public Callable<Callable> apply1(Object operand, LstEnv env, TCont cont)
    {
      return cont.call(operand);
    }

    public Callable<Callable> apply2(Object operand1, Object operand2, LstEnv env, TCont cont)
    {
      BigInteger i = (BigInteger) operand1;
      BigInteger j = (BigInteger) operand2;
      return cont.call(i.subtract(j));
    }

    public Callable<Callable> applyN(Object[] operands, LstEnv env, TCont cont)
    {
      BigInteger r = (BigInteger) operands[0];
      for (int i = 1; i < operands.length; i++)
      {
        r = r.subtract((BigInteger) operands[i]);
      }
      return cont.call(r);
    }
  }

  public static final class Not extends Procedure
  {
    public Callable<Callable> apply1(Object operand, LstEnv env, TCont cont)
    {
      return cont.call(Primitives.not(operand));
    }
  }

  public static final class Equals extends Procedure
  {
    public Callable<Callable> apply2(Object operand1, Object operand2, LstEnv env, TCont cont)
    {
      return cont.call(operand1.equals(operand2));
    }

    public Callable<Callable> applyN(Object[] operands, LstEnv env, TCont cont)
    {
      Object r = operands[0];
      for (int i = 1; i < operands.length; i++)
      {
        if (!r.equals(operands[i]))
        {
          return cont.call(Boolean.FALSE);
        }
      }
      return cont.call(Boolean.TRUE);
    }
  }

  public static final class IZeroP extends Procedure
  {
    public Callable<Callable> apply1(Object operand, LstEnv env, TCont cont)
    {
      return cont.call(operand.equals(IZERO));
    }
  }

  public static final class IIZeroP extends Procedure
  {
    public Callable<Callable> apply1(Object operand, LstEnv env, TCont cont)
    {
      return cont.call(operand.equals(BigInteger.ZERO));
    }
  }

  public static final class Length extends Procedure
  {
    public Callable<Callable> apply1(Object operand, LstEnv env, TCont cont)
    {
      return cont.call((int) ((Lst) operand).length());
    }
  }

  public static final class ListRef extends Procedure
  {
    public Callable<Callable> apply2(Object operand1, Object operand2, LstEnv env, TCont cont)
    {
      return cont.call(((Lst) operand1).listRef((Integer) operand2));
    }
  }

  public static final class Reverse extends Procedure
  {
    public Callable<Callable> apply1(Object operand, LstEnv env, TCont cont)
    {
      return cont.call(((Lst) operand).reverse());
    }
  }

  public static final class NullP extends Procedure
  {
    public Callable<Callable> apply1(Object operand, LstEnv env, TCont cont)
    {
      return cont.call(operand instanceof Null);
    }
  }

//  public static final class EmptyP extends Procedure
//  {
//    public Callable<Callable> apply1(Object operand, LstEnv env, TCont cont)
//    {
//      return cont.call(((FTree) operand).isEmpty());
//    }
//  }
//
//  public static final class SingletonP extends Procedure
//  {
//    public Callable<Callable> apply1(Object operand, LstEnv env, TCont cont)
//    {
//      return cont.call(operand instanceof Single);
//    }
//  }
//
//  public static final class DeepP extends Procedure
//  {
//    public Callable<Callable> apply1(Object operand, LstEnv env, TCont cont)
//    {
//      return cont.call(operand instanceof Deep);
//    }
//  }

  public static final class EqP extends Procedure
  {
    public Callable<Callable> apply2(Object operand1, Object operand2, LstEnv env, TCont cont)
    {
      return cont.call(Primitives.eq(operand1, operand2));
    }
  }

  public static final class EqvP extends Procedure
  {
    public Callable<Callable> apply2(Object operand1, Object operand2, LstEnv env, TCont cont)
    {
      return cont.call(Primitives.eqv(operand1, operand2));
    }
  }

  public static final class EqualP extends Procedure
  {
    public Callable<Callable> apply2(Object operand1, Object operand2, LstEnv env, TCont cont)
    {
      return cont.call(Primitives.equal(operand1, operand2));
    }
  }

  public static final class LessThan extends Procedure
  {
    public Callable<Callable> apply2(Object operand1, Object operand2, LstEnv env, TCont cont)
    {
      return cont.call(((Comparable) operand1).compareTo(operand2) < 0);
    }

    public Callable<Callable> applyN(Object[] operands, LstEnv env, TCont cont)
    {
      Comparable c = (Comparable) operands[0];
      for (int i = 1; i < operands.length; i++)
      {
        if (c.compareTo(operands[i]) > -1)
        {
          return cont.call(Boolean.FALSE);
        }
      }
      return cont.call(Boolean.TRUE);
    }
  }

  public static final class LessThanOrEquals extends Procedure
  {
    public Callable<Callable> apply2(Object operand1, Object operand2, LstEnv env, TCont cont)
    {
      return cont.call(((Comparable) operand1).compareTo(operand2) <= 0);
    }

    public Callable<Callable> applyN(Object[] operands, LstEnv env, TCont cont)
    {
      Comparable c = (Comparable) operands[0];
      for (int i = 1; i < operands.length; i++)
      {
        if (c.compareTo(operands[i]) > 0)
        {
          return cont.call(Boolean.FALSE);
        }
      }
      return cont.call(Boolean.TRUE);
    }
  }

  public static final class GreaterThan extends Procedure
  {
    public Callable<Callable> apply2(Object operand1, Object operand2, LstEnv env, TCont cont)
    {
      return cont.call(((Comparable) operand1).compareTo(operand2) > 0);
    }

    public Callable<Callable> applyN(Object[] operands, LstEnv env, TCont cont)
    {
      Comparable c = (Comparable) operands[0];
      for (int i = 1; i < operands.length; i++)
      {
        if (c.compareTo(operands[i]) < 1)
        {
          return cont.call(Boolean.FALSE);
        }
      }
      return cont.call(Boolean.TRUE);
    }
  }

  public static final class GreaterThanOrEquals extends Procedure
  {
    public Callable<Callable> apply2(Object operand1, Object operand2, LstEnv env, TCont cont)
    {
      return cont.call(((Comparable) operand1).compareTo(operand2) >= 0);
    }

    public Callable<Callable> applyN(Object[] operands, LstEnv env, TCont cont)
    {
      Comparable c = (Comparable) operands[0];
      for (int i = 1; i < operands.length; i++)
      {
        if (c.compareTo(operands[i]) < 0)
        {
          return cont.call(Boolean.FALSE);
        }
      }
      return cont.call(Boolean.TRUE);
    }
  }

//  public static final class Conc extends Procedure
//  {
//    public Callable<Callable> apply2(Object operand1, Object operand2, LstEnv env, TCont cont)
//    {
//      return cont.call(((FTree) operand1).append((FTree) operand2));
//    }
//  }
//
//  public static final class Cached extends Procedure
//  {
//    public Callable<Callable> apply1(Object operand, LstEnv env, TCont cont)
//    {
//      return cont.call(((FTree) operand).cached());
//    }
//  }
//
//  public static final class TPSum extends Procedure
//  {
//    public Callable<Callable> apply1(Object tree, LstEnv env, TCont cont)
//    {
//      FTree t = (FTree) tree;
//      final Measure m = t.measure();
//      return cont.call(t.psum());
//    }
//  }
//
//  public static final class LPSum extends Procedure
//  {
//    public Callable<Callable> applyN(Object[] operands, LstEnv env, TCont cont)
//    {
//      Object empty = operands[0];
//      Procedure sum = (Procedure) operands[1];
//      Procedure measure = (Procedure) operands[2];
//      Lst l = (Lst) operands[3];
//      final Measure m = createMeasure(empty, sum, measure, env);
//      Object i = m.empty();
//      Lst resultL = Lst.valueOf(i);
//      for (Object o : l)
//      {
//        i = m.sum(m.measure(o), i);
//        resultL = Pair.cons(i, resultL);
//      }
//      resultL = resultL.reverse();
//      return cont.call(resultL);
//    }
//  }
//
//  public static final class LPSumArray extends Procedure
//  {
//    public Callable<Callable> applyN(Object[] operands, LstEnv env, TCont cont)
//    {
//      Object empty = operands[0];
//      Procedure sum = (Procedure) operands[1];
//      Procedure measure = (Procedure) operands[2];
//      Lst l = (Lst) operands[3];
//      final Measure m = createMeasure(empty, sum, measure, env);
//      Object i = m.empty();
//      java.util.List result = new ArrayList();
//      result.add(i);
//      for (Object o : l)
//      {
//        i = m.sum(m.measure(o), i);
//        result.add(i);
//      }
//      return cont.call(Lst.valueOf(result));
//    }
//  }
//
//  public static final class TPPSum extends Procedure
//  {
//    private ExecutorService executor;
//
//    public TPPSum(ExecutorService executor)
//    {
//      super();
//      this.executor = executor;
//    }
//
//    public Callable<Callable> apply1(Object tree, LstEnv env, TCont cont)
//    {
//      FTree t = (FTree) tree;
//      final Measure m = t.measure();
//      return cont.call(t.ppsum(executor));
//    }
//  }
//
//  public static final class TMap extends Procedure
//  {
//    private StremeContext context;
//
//    public TMap(StremeContext context)
//    {
//      super();
//      this.context = context;
//    }
//
//    public Callable<Callable> apply2(Object tree, final Object mapProc, final LstEnv env, TCont cont)
//    {
//      FTree t = (FTree) tree;
//      t = t.map(t.measure(), new Mapper()
//      {
//        public Object map(Object v)
//        {
//          return javaApply1((Procedure) mapProc, v, env);
//        }
//      });
//      return cont.call(t);
//    }
//  }

//  public static final class Left extends Procedure
//  {
//    public Callable<Callable> apply1(Object operand, LstEnv env, TCont cont)
//    {
//      return cont.call(((FTree) operand).leftHead());
//    }
//  }
//
//  public static final class LeftTail extends Procedure
//  {
//    public Callable<Callable> apply1(Object operand, LstEnv env, TCont cont)
//    {
//      return cont.call(((FTree) operand).leftTail());
//    }
//  }
//
//  public static final class AddLeft extends Procedure
//  {
//    public Callable<Callable> apply2(Object operand1, Object operand2, LstEnv env, TCont cont)
//    {
//      return cont.call(((FTree) operand1).addLeft(operand2));
//    }
//  }
//
//  public static final class Right extends Procedure
//  {
//    public Callable<Callable> apply1(Object operand, LstEnv env, TCont cont)
//    {
//      return cont.call(((FTree) operand).rightHead());
//    }
//  }
//
//  public static final class AddRight extends Procedure
//  {
//    public Callable<Callable> apply2(Object operand1, Object operand2, LstEnv env, TCont cont)
//    {
//      return cont.call(((FTree) operand1).addRight(operand2));
//    }
//  }
//
//  public static final class RightTail extends Procedure
//  {
//    public Callable<Callable> apply1(Object operand, LstEnv env, TCont cont)
//    {
//      return cont.call(((FTree) operand).rightTail());
//    }
//  }

  public static final class Cons extends Procedure
  {
    public Callable<Callable> apply2(Object operand1, Object operand2, LstEnv env, TCont cont)
    {
      return cont.call(Pair.cons(operand1, operand2));
    }
  }

  public static final class List extends Procedure
  {
    public Callable<Callable> apply0(LstEnv env, TCont cont)
    {
      return cont.call(new Null());
    }

    public Callable<Callable> apply1(Object operand, LstEnv env, TCont cont)
    {
      return cont.call(Pair.cons(operand, new Null()));
    }

    public Callable<Callable> apply2(Object operand1, Object operand2, LstEnv env, TCont cont)
    {
      return cont.call(Pair.cons(operand1, Pair.cons(operand2, new Null())));
    }

    public Callable<Callable> apply3(Object operand1, Object operand2, Object operand3, LstEnv env, TCont cont)
    {
      return cont.call(Pair.cons(operand1, Pair.cons(operand2, Pair.cons(operand3, new Null()))));
    }

    public Callable<Callable> applyN(Object[] operands, LstEnv env, TCont cont)
    {
      return cont.call(Lst.valueOf(operands));
    }
  }

  private static Object javaApply1(Procedure proc, Object operand, LstEnv env)
  {
    final Object[] result = new Object[1];
    TCont.trampoline(proc.apply1(operand, env, new TCont()
    {
      public Callable<Callable> call(Object value)
      {
        result[0] = value;
        return null;
      }
    }));
    return result[0];
  }

  private static Object javaApply2(Procedure proc, Object operand1, Object operand2, LstEnv env)
  {
    final Object[] result = new Object[1];
    TCont.trampoline(proc.apply2(operand1, operand2, env, new TCont()
    {
      public Callable<Callable> call(Object value)
      {
        result[0] = value;
        return null;
      }
    }));
    return result[0];
  }

//  private static Measure createMeasure(final Object empty, final Procedure s, final Procedure m, final LstEnv env)
//  {
//    Measure mm = new Measure()
//    {
//      public Object measure(Object t)
//      {
//        return javaApply1(m, t, env);
//      }
//
//      public Object empty()
//      {
//        return empty;
//      }
//
//      public Object sum(Object a, Object b)
//      {
//        final Object[] result = new Object[1];
//        return javaApply2(s, a, b, env);
//      }
//    };
//    return mm;
//  }

//  public static final class Tree extends Procedure
//  {
//    private StremeContext context;
//
//    public Tree(StremeContext context)
//    {
//      super();
//      this.context = context;
//    }
//
//    public Callable<Callable> apply0(LstEnv env, TCont cont)
//    {
//      return cont.call(FTree.treeOf());
//    }
//
//    public Callable<Callable> apply3(final Object empty, Object sum, Object measure, final LstEnv env, TCont cont)
//    {
//      final Procedure m = (Procedure) measure;
//      final Procedure s = (Procedure) sum;
//      Measure mm = createMeasure(empty, s, m, env);
//      return cont.call(FTree.treeOf(mm));
//    }
//
//    public Callable<Callable> applyN(Object[] operands, LstEnv env, TCont cont)
//    {
//      return cont.call(FTree.treeOf(Measure.size(), operands));
//    }
//  }
//
//  public static final class Split extends Procedure
//  {
//    public Callable<Callable> applyN(Object[] operands, final LstEnv env, TCont cont)
//    {
//      FTree tree = (FTree) operands[0];
//      final Procedure predicate = (Procedure) operands[1];
//      Object initial = operands[2];
//      Procedure receiver = (Procedure) operands[3];
//      ftree.Split split = tree.split(new Predicate()
//      {
//        public boolean apply(Object v)
//        {
//          return javaApply1(predicate, v, env) == Boolean.FALSE ? false : true;
//        }
//      }, initial);
//      if (split == null)
//      {
//        return receiver.apply2(tree, tree.clear(), env, cont);
//      }
//      else
//      {
//        return receiver.apply2(split.getLeft(), split.getRight().addLeft(split.getCenter()), env, cont);
//      }
//    }
//  }
//
//  public static final class Equip extends Procedure
//  {
//    public Callable<Callable> apply2(Object t, Object protoTree, final LstEnv env, TCont cont)
//    {
//      FTree tree = ((FTree) protoTree).clear();
//      for (Object o : (FTree) t)
//      {
//        tree = tree.addRight(o);
//      }
//      return cont.call(tree);
//    }
//  }
//
//  public static final class List2Tree extends Procedure
//  {
//    public Callable<Callable> apply1(Object iterable, final LstEnv env, TCont cont)
//    {
//      return cont.call(FTree.fromIterable(Measure.nil(), (Iterable) iterable));
//    }
//
//    public Callable<Callable> apply2(Object iterable, Object proto, final LstEnv env, TCont cont)
//    {
//      return cont.call(FTree.fromIterable(((FTree) proto).measure(), (Iterable) iterable));
//    }
//  }

  public static final class ImproperList extends Procedure
  {
    public Callable<Callable> apply2(Object operand1, Object operand2, LstEnv env, TCont cont)
    {
      return cont.call(Pair.cons(operand1, operand2));
    }

    public Callable<Callable> apply3(Object operand1, Object operand2, Object operand3, LstEnv env, TCont cont)
    {
      return cont.call(Pair.cons(operand1, Pair.cons(operand2, operand3)));
    }

    public Callable<Callable> applyN(Object[] operands, LstEnv env, TCont cont)
    {
      return cont.call(Lst.valueOfImproper(operands));
    }
  }

  public static final class Map extends Procedure
  {
    public Callable<Callable> apply2(Object operator, Object list, final LstEnv env, final TCont cont)
    {
      if (list instanceof Null)
      {
        return cont.call(new Null());
      }
      final Procedure op = (Procedure) operator;
      final Pair lst = (Pair) list;
      return op.apply1(lst.car(), env, new TCont()
      {
        private java.util.List<Object> mapped = new ArrayList<Object>();
        private Lst l = (Lst) lst.cdr();

        public Callable<Callable> call(Object value)
        {
          mapped.add(value);
          if (l.isNull())
          {
            return cont.call(Lst.valueOf(mapped));
          }
          else
          {
            Object car = l.car();
            l = (Lst) l.cdr();
            return op.apply1(car, env, this);
          }
        }
      });
    }
    
    public Callable<Callable> apply3(Object operator, Object l1, Object l2, final LstEnv env, final TCont cont)
    {
      if (l1 instanceof Null)
      {
        return cont.call(new Null());
      }
      final Procedure op = (Procedure) operator;
      final Pair lst1 = (Pair) l1;
      final Pair lst2 = (Pair) l2;
      return op.apply2(lst1.car(), lst2.car(), env, new TCont()
      {
        private java.util.List<Object> mapped = new ArrayList<Object>();
        private Lst l1 = (Lst) lst1.cdr();
        private Lst l2 = (Lst) lst2.cdr();

        public Callable<Callable> call(Object value)
        {
          mapped.add(value);
          if (l1.isNull())
          {
            return cont.call(Lst.valueOf(mapped));
          }
          else
          {
            Object car1 = l1.car();
            Object car2 = l2.car();
            l1 = (Lst) l1.cdr();
            l2 = (Lst) l2.cdr();
            return op.apply2(car1, car2, env, this);
          }
        }
      });
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

    public Callable<Callable> apply2(Object operator, Object list, final LstEnv env, final TCont cont)
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
            TCont.trampoline(proc.apply1(els[j], env, new TCont()
            {
              public Callable<Callable> call(Object val)
              {
                nels[j] = val;
                return null;
              }
            }));
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
      return cont.call(Lst.valueOf(nels));
    }
    // TODO other apply2, ..., applyN
  }

  public static final class ForEach extends Procedure
  {
    public Callable<Callable> apply2(Object operator, Object list, final LstEnv env, final TCont cont)
    {
      if (list instanceof Null)
      {
        return cont.call(new Null());
      }
      final Procedure op = (Procedure) operator;
      final Pair lst = (Pair) list;
      return op.apply1(lst.car(), env, new TCont()
      {
        private Lst l = (Lst) lst.cdr();

        public Callable<Callable> call(Object value)
        {
          if (l.isNull())
          {
            return cont.call(Literal.UNSPECIFIED);
          }
          else
          {
            Object car = l.car();
            l = (Lst) l.cdr();
            return op.apply1(car, env, this);
          }
        }
      });
    }
    // TODO other apply2, ..., applyN
  }

  public static final class Append extends Procedure
  {
    public Callable<Callable> apply0(LstEnv env, TCont cont)
    {
      return cont.call(Primitives.append());
    }
    
    public Callable<Callable> apply1(Object operand, LstEnv env, TCont cont)
    {
      return cont.call(Primitives.append(operand));
    }
    
    public Callable<Callable> apply2(Object operand1, Object operand2, LstEnv env, TCont cont)
    {
      return cont.call(Primitives.append(operand1, operand2));
    }

    public Callable<Callable> apply3(Object operand1, Object operand2, Object operand3, LstEnv env, TCont cont)
    {
      return cont.call(Primitives.append(operand1, operand2, operand3));
    }
    
    public Callable<Callable> applyN(Object[] operands, LstEnv env, TCont cont)
    {
      return cont.call(Primitives.append(operands));
    }
  }

  public static final class Memv extends Procedure
  {
    public Callable<Callable> apply2(Object candidate, Object list, LstEnv env, TCont cont)
    {
      return cont.call(Primitives.memv(candidate, list));
    }
  }

  public static final class Memq extends Procedure
  {
    public Callable<Callable> apply2(Object candidate, Object list, LstEnv env, TCont cont)
    {
      return cont.call(Primitives.memq(candidate, list));
    }
  }

  public static final class Member extends Procedure
  {
    public Callable<Callable> apply2(Object candidate, Object list, LstEnv env, TCont cont)
    {
      return cont.call(Primitives.member(candidate, list));
    }
  }

  public static final class Member2 extends Procedure
  {
    public Callable<Callable> apply2(Object candidate, Object list, LstEnv env, TCont cont)
    {
      return cont.call(Primitives.member(candidate, list));
    }
  }

  public static final class Assoc extends Procedure // UNTESTED
  {
    public Callable<Callable> apply2(Object candidate, Object list, LstEnv env, TCont cont)
    {
      while (!(list instanceof Null))
      {
        Pair p = (Pair) list;
        Pair c = (Pair) p.car();
        if (Primitives.equal(candidate, c.car()))
        {
          return cont.call(c);
        }
        list = p.cdr();
      }
      return cont.call(Boolean.FALSE);
    }
  }

  public static final class Assq extends Procedure // UNTESTED
  {
    public Callable<Callable> apply2(Object candidate, Object list, LstEnv env, TCont cont)
    {
      while (!(list instanceof Null))
      {
        Pair p = (Pair) list;
        Pair c = (Pair) p.car();
        if (Primitives.eq(candidate, c.car()))
        {
          return cont.call(c);
        }
        list = p.cdr();
      }
      return cont.call(Boolean.FALSE);
    }
  }

  public static final class String2Symbol extends Procedure // UNTESTED
  {
    public Callable<Callable> apply1(Object operand, LstEnv env, TCont cont)
    {
      return cont.call(new Sym((String) operand));
    }
  }

  public static final class StringRef extends Procedure // UNTESTED
  {
    public Callable<Callable> apply2(Object string, Object index, LstEnv env, TCont cont)
    {
      return cont.call(((String) string).charAt((Integer) index));
    }
  }

  public static final class Symbol2String extends Procedure // UNTESTED
  {
    public Callable<Callable> apply1(Object operand, LstEnv env, TCont cont)
    {
      return cont.call(((Sym) operand).getName());
    }
  }

  public static final class Number2String extends Procedure // UNTESTED
  {
    public Callable<Callable> apply1(Object operand, LstEnv env, TCont cont)
    {
      return cont.call(operand.toString());
    }
  }

  public static final class List2Vector extends Procedure // UNTESTED
  {
    public Callable<Callable> apply1(Object operand, LstEnv env, TCont cont)
    {
      return cont.call(((Lst) operand).properToArray());
    }
  }

  public static final class Vector2List extends Procedure // UNTESTED
  {
    public Callable<Callable> apply1(Object operand, LstEnv env, TCont cont)
    {
      return cont.call(Lst.valueOf((Object[]) operand));
    }
  }

  public static final class ToF extends Procedure // UNTESTED
  {
    public Callable<Callable> apply1(Object operand, LstEnv env, TCont cont)
    {
      return cont.call(((Number) operand).doubleValue());
    }
  }

  public static final class StringAppend extends Procedure // UNTESTED
  {
    public Callable<Callable> apply2(Object operand1, Object operand2, LstEnv env, TCont cont)
    {
      return cont.call(((String) operand1).concat((String) operand2));
    }

    public Callable<Callable> apply3(Object operand1, Object operand2, Object operand3, LstEnv env, TCont cont)
    {
      return cont.call(((String) operand1).concat((String) operand2));
    }
  }

  public static final class StringLength extends Procedure // UNTESTED
  {
    public Callable<Callable> apply1(Object operand, LstEnv env, TCont cont)
    {
      return cont.call(((String) operand).length());
    }
  }

  public static final class NumberP extends Procedure // UNTESTED
  {
    public Callable<Callable> apply1(Object operand, LstEnv env, TCont cont)
    {
      return cont.call(operand instanceof Number || operand instanceof BigRational);
    }
  }

  public static final class PrimitiveP extends Procedure // UNTESTED
  {
    public Callable<Callable> apply1(Object operand, LstEnv env, TCont cont)
    {
      return cont.call(new Exception("TODO"));
    }
  }

  public static final class ClassForName extends Procedure // UNTESTED
  {
    public Callable<Callable> apply1(Object operand, LstEnv env, TCont cont)
    {
      return cont.call(Primitives.classForName(operand));
    }
  }

  public static final class New extends Procedure // UNTESTED
  {
    public Callable<Callable> apply1(Object clazz, LstEnv env, TCont cont)
    {
      try
      {
        return cont.call(ConstructorUtils.invokeConstructor((Class) clazz, null));
      }
      catch (Exception e)
      {
        throw new StremeException("reflection error", e);
      }
    }

    public Callable<Callable> apply2(Object clazz, Object arg, LstEnv env, TCont cont)
    {
      try
      {
        return cont.call(ConstructorUtils.invokeConstructor((Class) clazz, arg));
      }
      catch (Exception e)
      {
        throw new StremeException("reflection error", e);
      }
    }

    public Callable<Callable> apply3(Object clazz, Object arg1, Object arg2, LstEnv env, TCont cont)
    {
      try
      {
        return cont.call(ConstructorUtils.invokeConstructor((Class) clazz, new Object[] { arg1, arg2}));
      }
      catch (Exception e)
      {
        throw new StremeException("reflection error", e);
      }
    }

    public Callable<Callable> applyN(Object[] operands, LstEnv env, TCont cont)
    {
      Class<?> clazz = (Class) operands[0];
      Object[] args = Arrays.copyOfRange(operands, 1, operands.length);
      try
      {
        return cont.call(ConstructorUtils.invokeConstructor(clazz, args));
      }
      catch (Exception e)
      {
        throw new StremeException("reflection error", e);
      }
    }
  }

  public static final class Invoke extends Procedure // UNTESTED
  {
    public Callable<Callable> apply2(Object instance, Object instanceMember, LstEnv env, TCont cont)
    {
      try
      {
        try
        {
          Object value = MethodUtils.invokeMethod(instance, String.valueOf(instanceMember), null);
          return cont.call(value);
        }
        catch (NoSuchMethodException nsme)
        {
          try
          {
            Field field = instance.getClass().getField(String.valueOf(instanceMember));
            Object value = field.get(instance);
            return cont.call(value);
          }
          catch (NoSuchFieldException nsfe)
          {
            throw new StremeException(instanceMember + ": no such method or field on " + instance, nsfe);
          }
        }
      }
      catch (Exception e)
      {
        throw new StremeException("reflection error", e);
      }
    }

    public Callable<Callable> apply3(Object instance, Object instanceMember, Object arg, LstEnv env, TCont cont)
    {
      try
      {
        Object value = MethodUtils.invokeMethod(instance, String.valueOf(instanceMember), arg);
        return cont.call(value);
      }
      catch (Exception e)
      {
        throw new StremeException("reflection error", e);
      }
    }

    public Callable<Callable> applyN(Object[] operands, LstEnv env, TCont cont)
    {
      Object instance = operands[0];
      String instanceMember = String.valueOf(operands[1]);
      Object[] args = Arrays.copyOfRange(operands, 2, operands.length);
      try
      {
        Object value = MethodUtils.invokeMethod(instance, instanceMember, args);
        return cont.call(value);
      }
      catch (Exception e)
      {
        throw new StremeException("reflection error", e);
      }
    }
  }

  public static final class InvokeStatic extends Procedure // UNTESTED
  {
    public Callable<Callable> apply2(Object clazz, Object staticMember, LstEnv env, TCont cont)
    {
      try
      {
        try
        {
          Object value = MethodUtils.invokeStaticMethod((Class) clazz, String.valueOf(staticMember), null);
          return cont.call(value);
        }
        catch (NoSuchMethodException nsme)
        {
          try
          {
            Field field = ((Class) clazz).getField(String.valueOf(staticMember));
            Object value = field.get(null);
            return cont.call(value);
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

    public Callable<Callable> apply3(Object clazz, Object staticMember, Object arg, LstEnv env, TCont cont)
    {
      try
      {
        Object value = MethodUtils.invokeStaticMethod((Class) clazz, String.valueOf(staticMember), arg);
        return cont.call(value);
      }
      catch (Exception e)
      {
        throw new StremeException("reflection error", e);
      }
    }

    public Callable<Callable> applyN(Object[] operands, LstEnv env, TCont cont)
    {
      Class<?> clazz = (Class) operands[0];
      String staticMember = String.valueOf(operands[1]);
      Object[] args = Arrays.copyOfRange(operands, 2, operands.length);
      try
      {
        Object value = MethodUtils.invokeStaticMethod(clazz, staticMember, args);
        return cont.call(value);
      }
      catch (Exception e)
      {
        throw new StremeException("reflection error", e);
      }
    }
  }

  public static final class Proxy extends Procedure // UNTESTED
  {
    private StremeContext context;

    public Proxy(StremeContext context)
    {
      super();
      this.context = context;
    }

    public Callable<Callable> apply2(Object interfaceNames, final Object bindings, final LstEnv env, final TCont cont)
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
              if (assoc == Boolean.FALSE)
              {
                throw new StremeException("cannot invoke " + name + " on proxy " + proxy);
              }
              Procedure proc = (Procedure) ((Pair) assoc).cdr();
              final Object[] result = new Object[1];
              TCont rc = new TCont()
              {
                public Callable<Callable> call(Object value)
                {
                  result[0] = value;
                  return null;
                }
              };
              switch (args.length)
              {
                case 0:
                  TCont.trampoline(proc.apply0(env, rc));
                  break;
                case 1:
                  TCont.trampoline(proc.apply1(args[0], env, rc));
                  break;
                case 2:
                  TCont.trampoline(proc.apply2(args[0], args[1], env, rc));
                  break;
                case 3:
                  TCont.trampoline(proc.apply3(args[0], args[1], args[2], env, rc));
                  break;
                default:
                  TCont.trampoline(proc.applyN(args, env, rc));
                  break;
              }
              return result[0];
            }
          });
      return cont.call(proxy);
    }
  }

  public static final class GetField extends Procedure // UNTESTED
  {
    public Callable<Callable> apply2(Object instance, Object instanceMember, LstEnv env, TCont cont)
    {
      try
      {
        Field field = instance.getClass().getField(String.valueOf(instanceMember));
        Object value = field.get(instance);
        return cont.call(value);
      }
      catch (Exception e)
      {
        throw new StremeException("reflection error", e);
      }
    }
  }

  public static final class GetStaticField extends Procedure // UNTESTED
  {
    public Callable<Callable> apply2(Object clazz, Object staticMember, LstEnv env, TCont cont)
    {
      try
      {
        Field field = ((Class) clazz).getField(String.valueOf(staticMember));
        Object value = field.get(null);
        return cont.call(value);
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
    public Callable<Callable> apply1(Object operand, LstEnv env, TCont cont)
    {
      return cont.call(operand instanceof Sym);
    }
  }

  public static final class PairP extends Procedure// UNTESTED
  {
    public Callable<Callable> apply1(Object operand, LstEnv env, TCont cont)
    {
      return cont.call(operand instanceof Pair);
    }
  }

  public static final class ListP extends Procedure// UNTESTED
  {
    public Callable<Callable> apply1(Object operand, LstEnv env, TCont cont)
    {
      return cont.call(operand instanceof Lst);
    }
  }

//  public static final class TreeP extends Procedure// UNTESTED
//  {
//    public Callable<Callable> apply1(Object operand, LstEnv env, TCont cont)
//    {
//      return cont.call(operand instanceof FTree);
//    }
//  }

  public static final class Car extends Procedure
  {
    public Callable<Callable> apply1(Object operand, LstEnv env, TCont cont)
    {
      return cont.call(((Lst) operand).car());
    }
  }

  public static final class Caadr extends Procedure // UNTESTED
  {
    public Callable<Callable> apply1(Object operand, LstEnv env, TCont cont)
    {
      return cont.call(((Lst) operand).caadr());
    }
  }

  public static final class Cadadr extends Procedure // UNTESTED
  {
    public Callable<Callable> apply1(Object operand, LstEnv env, TCont cont)
    {
      return cont.call(((Lst) operand).cadadr());
    }
  }

  public static final class Cadddr extends Procedure // UNTESTED
  {
    public Callable<Callable> apply1(Object operand, LstEnv env, TCont cont)
    {
      return cont.call(((Lst) operand).cadddr());
    }
  }

  public static final class Caddr extends Procedure // UNTESTED
  {
    public Callable<Callable> apply1(Object operand, LstEnv env, TCont cont)
    {
      return cont.call(((Lst) operand).caddr());
    }
  }

  public static final class Cadr extends Procedure // UNTESTED
  {
    public Callable<Callable> apply1(Object operand, LstEnv env, TCont cont)
    {
      return cont.call(((Lst) operand).cadr());
    }
  }

  public static final class Cdar extends Procedure // UNTESTED
  {
    public Callable<Callable> apply1(Object operand, LstEnv env, TCont cont)
    {
      return cont.call(((Lst) operand).cdar());
    }
  }

  public static final class Caar extends Procedure // UNTESTED
  {
    public Callable<Callable> apply1(Object operand, LstEnv env, TCont cont)
    {
      return cont.call(((Lst) operand).caar());
    }
  }

  public static final class Cddr extends Procedure // UNTESTED
  {
    public Callable<Callable> apply1(Object operand, LstEnv env, TCont cont)
    {
      return cont.call(((Lst) operand).cddr());
    }
  }

  public static final class Cdddr extends Procedure // UNTESTED
  {
    public Callable<Callable> apply1(Object operand, LstEnv env, TCont cont)
    {
      return cont.call(((Lst) operand).cdddr());
    }
  }

  public static final class SetCdrB extends Procedure // UNTESTED
  {
    public Callable<Callable> apply2(Object pair, Object value, LstEnv env, TCont cont)
    {
      ((Pair) pair).setCdr(value);
      return cont.call(Void.TYPE);
    }
  }

  public static final class SetCarB extends Procedure // UNTESTED
  {
    public Callable<Callable> apply2(Object pair, Object value, LstEnv env, TCont cont)
    {
      ((Pair) pair).setCar(value);
      return cont.call(Void.TYPE);
    }
  }

  public static final class Cdr extends Procedure
  {
    public Callable<Callable> apply1(Object operand, LstEnv env, TCont cont)
    {
      return cont.call(((Lst) operand).cdr());
    }
  }

  public static final class Display extends Procedure
  {
    public Callable<Callable> apply1(Object operand, LstEnv env, TCont cont)
    {
      System.out.print(Data.toString(operand));
      return cont.call(Void.TYPE);
    }
  }

  public static final class Eval extends Procedure // UNTESTED
  {
    private StremeContext context;

    public Eval(StremeContext context)
    {
      super();
      this.context = context;
    }

    /**
     * Eval in current environment: not R5RS
     */
    public Callable<Callable> apply1(Object operand, LstEnv env, TCont cont)
    {
      return context.evaluateData(operand, env, cont);
    }

    public Callable<Callable> apply2(Object operand, Object evalEnv, LstEnv env, TCont cont)
    {
      return context.evaluateData(operand, (LstEnv) evalEnv, cont);
    }
  }

  public static final class Apply extends Procedure // UNTESTED
  {
    public Callable<Callable> apply2(Object operator, Object operands, LstEnv env, TCont cont)
    {
//      System.out.println("apply " + operator + " " + operands);
      Procedure proc = (Procedure) operator;
      Object[] ops = ((Lst) operands).properToArray();
      switch (ops.length)
      {
        case 0:
          return proc.apply0(env, cont);
        case 1:
          return proc.apply1(ops[0], env, cont);
        case 2:
          return proc.apply2(ops[0], ops[1], env, cont);
        case 3:
          return proc.apply3(ops[0], ops[1], ops[2], env, cont);
        default:
          return proc.applyN(ops, env, cont);
      }
    }
  }

  public static final class Newline extends Procedure
  {
    public Callable<Callable> apply0(LstEnv env, TCont cont)
    {
      System.out.println();
      return cont.call(Void.TYPE);
    }
  }

  public static final class InteractionEnvironment extends Procedure
  {
    private StremeContext context;

    public InteractionEnvironment(StremeContext context)
    {
      super();
      this.context = context;
    }

    public Callable<Callable> apply0(LstEnv env, TCont cont)
    {
      return cont.call(context.globalEnv());
    }
  }

  public static final class CurrentEnvironment extends Procedure
  {
    public Callable<Callable> apply0(LstEnv env, TCont cont)
    {
      return cont.call(env);
    }
  }

  public static final class Read extends Procedure
  {
    private StremeContext context;

    public Read(StremeContext context)
    {
      super();
      this.context = context;
    }

    public Callable<Callable> apply0(LstEnv env, TCont cont)
    {
      return cont.call(context.read(new InputStreamReader(System.in)));
    }
  }

  public static final class StringLessThanP extends Procedure
  {
    public Callable<Callable> apply2(Object operand1, Object operand2, LstEnv env, TCont cont)
    {
      return cont.call(Primitives.stringLtP(operand1, operand2));
    }
  }

  public static final class StringLessThanEqualP extends Procedure
  {
    public Callable<Callable> apply2(Object operand1, Object operand2, LstEnv env, TCont cont)
    {
      return cont.call(Primitives.stringLteP(operand1, operand2));
    }
  }

  public static final class StringGreaterThanP extends Procedure
  {
    public Callable<Callable> apply2(Object operand1, Object operand2, LstEnv env, TCont cont)
    {
      return cont.call(Primitives.stringGtP(operand1, operand2));
    }
  }

  public static final class StringGreaterThanEqualP extends Procedure
  {
    public Callable<Callable> apply2(Object operand1, Object operand2, LstEnv env, TCont cont)
    {
      return cont.call(Primitives.stringGteP(operand1, operand2));
    }
  }

  public static final class Touch extends Procedure // UNTESTED
  {
    public Callable<Callable> apply1(Object operand, LstEnv env, TCont cont)
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
          return cont.call(value);
        }
        catch (Exception e)
        {
          throw new StremeException("error touching " + operand, e);
        }
      }
      else
      {
        return cont.call(operand);
      }
    }
    // public Callable<Callable> apply2(Object operand, Object onResolveObject, final LstEnv env, TSuccess cont, TCont
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
    private StremeContext context;

    public Load(StremeContext context)
    {
      super();
      this.context = context;
    }

    public Callable<Callable> apply1(Object operand, LstEnv env, final TCont cont)
    {
      final Reader reader;
      try
      {
        reader = new InputStreamReader(getClass().getResourceAsStream(operand.toString()));
        return load(reader, cont);
      }
      catch (Exception e)
      {
        throw new StremeException("error loading " + operand, e);
      }
    }

    private Callable<Callable> load(final Reader reader, final TCont cont)
    {
      Object parsed = context.read(reader);
      if (parsed != null)
      {
        return context.evaluateData(parsed, context.globalEnv(), new TCont()
        {
          public Callable<Callable> call(Object value)
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
              return cont.call(value);
            }
            else
            {
              return context.evaluateData(parsed, context.globalEnv(), this);
            }
          }
        });
      }
      else
      {
        return cont.call(Void.TYPE);
      }
    }
  }

  public static final class VectorSetB extends Procedure
  {
    public Callable<Callable> apply3(Object vector, Object index, Object value, LstEnv env, TCont cont)
    {
      ((Object[]) vector)[(Integer) index] = value;
      return cont.call(Void.TYPE);
    }
  }

  public static final class VectorRef extends Procedure
  {
    public Callable<Callable> apply2(Object vector, Object index, LstEnv env, TCont cont)
    {
      return cont.call(((Object[]) vector)[(Integer) index]);
    }
  }

  public static final class VectorLength extends Procedure
  {
    public Callable<Callable> apply1(Object vector, LstEnv env, TCont cont)
    {
      return cont.call(((Object[]) vector).length);
    }
  }

  public static final class MakeVector extends Procedure
  {
    public Callable<Callable> apply1(Object length, LstEnv env, TCont cont)
    {
      int l = (Integer) length;
      Object[] v = new Object[l];
      Arrays.fill(v, BigInteger.ZERO);
      return cont.call(v);
    }

    public Callable<Callable> apply2(Object length, Object init, LstEnv env, TCont cont)
    {
      int l = (Integer) length;
      Object[] v = new Object[l];
      Arrays.fill(v, init);
      return cont.call(v);
    }
  }

  public static final class VectorP extends Procedure
  {
    public Callable<Callable> apply1(Object operand, LstEnv env, TCont cont)
    {
      return cont.call(Primitives.vectorp(operand));
    }
  }

  
  public static final class Vector extends Procedure
  {
    public Callable<Callable> apply0(Object operand, LstEnv env, TCont cont)
    {
      return cont.call(new Object[0]);
    }

    public Callable<Callable> apply1(Object operand, LstEnv env, TCont cont)
    {
      return cont.call(new Object[] { operand});
    }

    public Callable<Callable> apply2(Object operand1, Object operand2, LstEnv env, TCont cont)
    {
      return cont.call(new Object[] { operand1, operand2});
    }

    public Callable<Callable> apply3(Object operand1, Object operand2, Object operand3, LstEnv env, TCont cont)
    {
      return cont.call(new Object[] { operand1, operand2, operand3});
    }

    public Callable<Callable> applyN(Object[] operands, LstEnv env, TCont cont)
    {
      return cont.call(operands);
    }
  }

  public static final class IModulo extends Procedure
  {
    public Callable<Callable> apply2(Object operand1, Object operand2, LstEnv env, TCont cont)
    {
      int i = (Integer) operand1;
      int j = (Integer) operand2;
      int rem = i % j;
      if (j < 0 && rem > 0)
      {
        return cont.call(rem + j);
      }
      else if (j > 0 && rem < 0)
      {
        return cont.call(rem + j);
      }
      else
      {
        return cont.call(rem);
      }
    }
  }

  public static final class IQuotient extends Procedure
  {
    public Callable<Callable> apply2(Object operand1, Object operand2, LstEnv env, TCont cont)
    {
      int i = (Integer) operand1;
      int j = (Integer) operand2;
      int r = i / j;
      return cont.call(r);
    }
  }

  public static final class NegativeP extends Procedure
  {
    public Callable<Callable> apply1(Object operand, LstEnv env, TCont cont)
    {
      if (operand instanceof Integer)
      {
        return cont.call(((Integer) operand) < 0);
      }
      if (operand instanceof BigInteger)
      {
        return cont.call(((BigInteger) operand).compareTo(BigInteger.ZERO) < 0);
      }
      throw new IllegalArgumentException("(negative? " + operand + ")");
    }
  }

  public static final class IOddP extends Procedure
  {
    public Callable<Callable> apply1(Object operand, LstEnv env, TCont cont)
    {
      return cont.call(((Integer) operand) % 2 != 0);
    }
  }

  public static final class IIOddP extends Procedure
  {
    public Callable<Callable> apply1(Object operand, LstEnv env, TCont cont)
    {
      return cont.call(((BigInteger) operand).mod(new BigInteger("2")) != BigInteger.ZERO);
    }
  }

  public static final class IEvenP extends Procedure
  {
    public Callable<Callable> apply1(Object operand, LstEnv env, TCont cont)
    {
      return cont.call(((Integer) operand) % 2 == 0);
    }
  }

  public static final class IIEvenP extends Procedure
  {
    public Callable<Callable> apply1(Object operand, LstEnv env, TCont cont)
    {
      return cont.call(((BigInteger) operand).mod(new BigInteger("2")) == BigInteger.ZERO);
    }
  }

  public static final class Unify extends Procedure
  {
    private final DataUnifier unifier = new DataUnifier();

    public Callable<Callable> apply2(Object operand1, Object operand2, LstEnv env, TCont cont)
    {
      java.util.Map<Sym, Object> result = unifier.unify(operand1, operand2);
      return cont.call(result == null ? Boolean.FALSE : Lst.assocValueOf(result));
    }
  }

  public static final class Sleep extends Procedure
  {
    public Callable<Callable> apply1(Object operand, LstEnv env, TCont cont)
    {
      try
      {
        Thread.sleep((Integer) operand);
      }
      catch (InterruptedException e)
      {
        throw new StremeException("sleep interrupted", e);
      }
      return cont.call(Void.TYPE);
    }
  }

  public static final class FixedRandomList extends Procedure
  {
    public Callable<Callable> apply1(Object operand, LstEnv env, TCont cont)
    {
      Random random = new Random(12345);
      Lst l = new Null();
      int m = (Integer) operand;
      for (int i = 0; i < m; i++)
      {
        l = Pair.cons(random.nextInt(), l);
      }
      return cont.call(l);
    }
  }

  // public static final class Serializer_ extends Procedure
  // {
  // public Callable<Callable> apply0(LstEnv env, TCont cont)
  // {
  // return cont.call(new Serializer());
  // }
  // }
  //
  // public static final class Serial extends Procedure
  // {
  // public Callable<Callable> apply2(Object operand1, Object operand2, final LstEnv env, final TSuccess cont, TCont
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
  // public Callable<Callable> apply1(Object operand, LstEnv env, TCont cont)
  // {
  // ((Serializer.Unlocker) operand).yield();
  // return cont.call(Void.TYPE);
  // }
  // }
  private SpattPrimitives()
  {
    super();
  }
}
