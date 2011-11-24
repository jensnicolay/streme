package streme.lang.ast.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import streme.lang.StremeException;
import streme.lang.ast.Application;
import streme.lang.ast.AstDataCompiler;
import streme.lang.ast.Begin;
import streme.lang.ast.Binding;
import streme.lang.ast.Define;
import streme.lang.ast.Future;
import streme.lang.ast.If;
import streme.lang.ast.Lambda;
import streme.lang.ast.Let;
import streme.lang.ast.Literal;
import streme.lang.ast.Literal.Kind;
import streme.lang.ast.Node;
import streme.lang.ast.Ref;
import streme.lang.ast.SetVar;
import streme.lang.ast.Var;
import streme.lang.data.Lst;
import streme.lang.data.Null;
import streme.lang.data.Pair;
import streme.lang.data.Parser2;
import streme.lang.data.Sym;

public class StremeDataCompiler implements AstDataCompiler
{
  
  public StremeDataCompiler()
  {
    super();
  }
  
  public Node compile(Object data)
  {
    if (data instanceof Pair)
    {
      Pair pair = (Pair) data;
      Object car = pair.car();
      if (car instanceof Sym)
      {
        Sym sym = (Sym) car;
        String name = sym.getName();
        if ("lambda".equals(name))
        {
          return compileLambda(pair);
        }
        if ("let".equals(name))
        {
          return compileLet(pair, Let.Kind.LET);
        }
        if ("let*".equals(name))
        {
          return compileLet(pair, Let.Kind.LETSTAR);
        }
        if ("letrec".equals(name))
        {
          return compileLet(pair, Let.Kind.LETREC);
        }
        if ("let||".equals(name))
        {
          return compileLet(pair, Let.Kind.LETPAR);
        }
        if ("begin".equals(name))
        {
          return compileBegin(pair);
        }
        if ("set!".equals(name))
        {
          return compileSetB(pair);
        }
        if ("if".equals(name))
        {
          return compileIf(pair);
        }
        if ("quote".equals(name))
        {
          return new Literal(pair.cadr(), Kind.QUOTE);
        }
        if ("define".equals(name))
        {
          return compileDefine(pair);
        }
        if ("future".equals(name))
        {
          return compileFuture(pair);
        }
      }
      return compileApplication(pair);
    }
    if (data instanceof Sym)
    {
      return compileSymbol(data);
    }
    return new Literal(data, Kind.CONSTANT);
  }

  private Node compileSymbol(Object source)
  {
    Sym sym = (Sym) source;
    return new Ref(sym);
  }

  private Node compileApplication(Pair pair)
  {
    Object car = pair.car();
    Node compiledCar = compile(car);
    Node[] operands = compileSequence((Lst) pair.cdr());
    return new Application(compiledCar, operands);
  }

  private Node compileLet(Pair pair, Let.Kind kind)
  {
    Pair pair2 = (Pair) pair.cdr();
    Lst bindings = (Lst) pair2.car();
    Lst body = (Lst) pair2.cdr();
    List<Binding> compiledBindings = new ArrayList<Binding>();
    for (Object bindingObj : bindings)
    {
      Pair binding = (Pair) bindingObj;
      compiledBindings.add(new Binding(new Var((Sym) binding.car()), compile(binding.cadr())));
    }
    return new Let(kind, compiledBindings.toArray(new Binding[compiledBindings.size()]), compileBody(body));
  }

  private Node compileDefine(Pair pair)
  {
    Pair pair2 = (Pair) pair.cdr();
    Object idObj = pair2.car();
    if (idObj instanceof Sym)
    {
      Sym id = (Sym) idObj;
      return new Define(new Var(id), compile(pair2.cadr()));
    }
    else
    {
      throw new StremeException("define: illegal syntax: " + pair);
    }
  }

  private Node compileFuture(Pair pair)
  {
    Pair pair2 = (Pair) pair.cdr();
    return new Future(compile(pair2.car()));
  }


  private Node compileIf(Pair pair)
  {
    Pair pair2 = (Pair) pair.cdr();
    Object cond = pair2.car();
    Node condition = compile(cond);
    pair2 = (Pair) pair2.cdr();
    Object consequent = pair2.car();
    Node consequentCompiled = compile(consequent);
    Object alternateList = pair2.cdr();
    Node alternateCompiled;
    if (alternateList instanceof Null)
    {
      alternateCompiled = Literal.UNSPECIFIED;
    }
    else
    {
      Pair pair3 = (Pair) alternateList;
      alternateCompiled = compile(pair3.car());
      if (!(pair3.cdr() instanceof Null))
      {
        throw new StremeException("if: more than 2 branches: " + pair);
      }
    }
    
    return new If(condition, consequentCompiled, alternateCompiled);
  }

  private Node compileSetB(Pair pair)
  {
    Pair pair2 = (Pair) pair.cdr();
    Sym id = (Sym) pair2.car();
    return new SetVar(new Var(id), compile(pair2.cadr()));
  }

  private Node compileBegin(Pair pair)
  {
    return new Begin(compileSequence((Lst) pair.cdr()), Begin.Kind.EXPLICIT);
  }

  private Node compileLambda(Pair pair)
  {
    Pair pair2 = (Pair) pair.cdr();
    Object paramsObj = pair2.car();
    Pair body = (Pair) pair2.cdr();
    Node compiledBody = compileBody(body);
    if (paramsObj instanceof Lst)
    {
      Pair arrayPair = ((Lst) paramsObj).toArray();
      Object[] params = (Object[]) arrayPair.cdr();
      Var[] varParams = new Var[params.length];
      for (int i = 0; i < params.length; i++)
      {
        Sym param = (Sym) params[i];
        varParams[i] = new Var(param);
      }
      if (Boolean.TRUE.equals(arrayPair.car()))
      {
        return new Lambda(varParams, compiledBody);
      }
      else
      {
        return new Lambda(Arrays.copyOf(varParams, params.length - 1), compiledBody, varParams[params.length - 1]);
      }
    }
    else
    {
      return new Lambda(new Var[0], compiledBody, new Var((Sym) paramsObj));
    }
  }

  private Node compileBody(Lst body)
  {
    Node[] compiledBody = compileSequence(body);
    Node actualBody = compiledBody.length == 1 ? compiledBody[0] : new Begin(compiledBody, Begin.Kind.IMPLICIT);
    return actualBody;
  }

  private Node[] compileSequence(Lst seq)
  {
    if (seq.isNull())
    {
      return new Node[0];
    }
    List<Node> compiled = new ArrayList<Node>();
    Object car = seq.car();
    seq = (Lst) seq.cdr();
    while (!seq.isNull())
    {
      compiled.add(compile(car));
      car = seq.car();
      seq = (Lst) seq.cdr();
    }
    compiled.add(compile(car));
    return compiled.toArray(new Node[compiled.size()]);
  }

  @Override
  public String toString()
  {
    return "Streme data2ast Compiler 0.1";
  }
  
  public static void main(String[] args)
  {
    Parser2 parser = new Parser2();
    Object data = parser.parse("'(1 2 3)");
    Node node = new StremeDataCompiler().compile(data);
    System.out.println(node);
  }
}
