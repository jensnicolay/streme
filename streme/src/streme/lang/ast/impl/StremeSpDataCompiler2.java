package streme.lang.ast.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;

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
import streme.lang.data.Lst.Mapper;
import streme.lang.data.Null;
import streme.lang.data.Pair;
import streme.lang.data.SpData;
import streme.lang.data.SpParser2;
import streme.lang.data.Sym;

public class StremeSpDataCompiler2 implements AstDataCompiler
{
  private Map<Object, SpData> sps;

  public StremeSpDataCompiler2(Map<Object, SpData> sps)
  {
    super();
    this.sps = sps;
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
          SpData spData = sps.get(pair);
          boolean datum = "'".equals(spData.getOriginal());
          Object value = pair.cadr();
          Literal literal = new Literal(value, datum ? Kind.DATUM : Kind.QUOTE);
          // literal.setProperty("sp", sps.get(value));
          if (value instanceof Pair)
          {
            Map<Object, SpData> localSps = new IdentityHashMap<Object, SpData>();
            scanLocalSps(value, sps, localSps);
            literal.setProperty("localSps", localSps);
          }
          return annotate(literal, spData);
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
    return annotate(new Literal(data, Kind.CONSTANT), sps.get(data));
  }
  
  public Node compileBody(Lst body)
  {
    Node[] compiledBody = compileSequence(body);
    Node actualBody;
    SpData sp1 = (SpData) compiledBody[0].getProperty("sp");
    SpData sp2 = (SpData) compiledBody[compiledBody.length - 1].getProperty("sp");
    SpData sp = new SpData(sp1.getPrefix(), sp1.getPos(), sp1.getLine(), sp1.getLinePos(), sp2.getEndPos()
        - sp1.getPos(), sp2.getSuffix());
    if (compiledBody.length == 1)
    {
      actualBody = compiledBody[0];
    }
    else
    {
      actualBody = new Begin(compiledBody, Begin.Kind.IMPLICIT);
      sp.setOriginal("");
    }
    return annotate(actualBody, sp);
  }

  private void scanLocalSps(Object value, final Map<Object, SpData> sps, final Map<Object, SpData> localSps)
  {
    localSps.put(value, sps.get(value));
    if (value instanceof Pair)
    {
      for (Object el : (Pair) value)
      {
        scanLocalSps(el, sps, localSps);
      }
    }
  }

  private Node compileSymbol(Object source)
  {
    Sym sym = (Sym) source;
    return annotate(new Ref(sym), sps.get(sym));
  }

  private Node compileApplication(Pair pair)
  {
    Object car = pair.car();
    Node compiledCar = compile(car);
    Node[] operands = compileSequence((Lst) pair.cdr());
    return annotate(new Application(compiledCar, operands), sps.get(pair));
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
      Sym var = (Sym) binding.car();
      compiledBindings.add(annotate(new Binding(annotate(new Var(var), sps.get(var)), compile(binding.cadr())),
          sps.get(binding)));
    }
    Let let = new Let(kind, compiledBindings.toArray(new Binding[compiledBindings.size()]), compileBody(body));
    let.setProperty("keywordSp", sps.get(pair.car()));
    let.setProperty("bindingsSp", sps.get(bindings));
    return annotate(let,
        sps.get(pair));
  }

  private Node compileDefine(Pair pair)
  {
    Pair pair2 = (Pair) pair.cdr();
    Object idObj = pair2.car();
    if (idObj instanceof Sym)
    {
      Sym id = (Sym) idObj;
      Object value = pair2.cadr();
      Define define = new Define(annotate(new Var(id), sps.get(id)), compile(value));
      define.setProperty("keywordSp", sps.get(pair.car()));
      return annotate(define, sps.get(pair));
    }
    else
    {
      throw new StremeException("define: illegal syntax: " + pair);
    }
  }

  private Node compileFuture(Pair pair)
  {
    Pair pair2 = (Pair) pair.cdr();
    return annotate(new Future(compile(pair2.car())), sps.get(pair));
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
        throw new StremeException("if: illegal syntax: " + pair);
      }
    }
    If ff = new If(condition, consequentCompiled, alternateCompiled);
    ff.setProperty("keywordSp", sps.get(pair.car()));
    return annotate(ff, sps.get(pair));
  }

  private Node compileSetB(Pair pair)
  {
    Pair pair2 = (Pair) pair.cdr();
    Sym id = (Sym) pair2.car();
    SetVar setVar = new SetVar(annotate(new Var(id), sps.get(id)), compile(pair2.cadr()));
    setVar.setProperty("keywordSp", sps.get(pair.car()));
    return annotate(setVar, sps.get(pair));
  }

  private Node compileBegin(Pair pair)
  {
    Begin begin = new Begin(compileSequence((Lst) pair.cdr()), Begin.Kind.EXPLICIT);
    begin.setProperty("keywordSp", sps.get(pair.car()));
    return annotate(begin, sps.get(pair));
  }

  private Node compileLambda(Pair pair)
  {
    Pair pair2 = (Pair) pair.cdr();
    Object paramsObj = pair2.car();
    Pair body = (Pair) pair2.cdr();
    Node compiledBody = compileBody(body);
    Lambda lambda;
    if (paramsObj instanceof Lst)
    {
      Pair arrayPair = ((Lst) paramsObj).toArray();
      Object[] params = (Object[]) arrayPair.cdr();
      Var[] varParams = new Var[params.length];
      for (int i = 0; i < params.length; i++)
      {
        Sym param = (Sym) params[i];
        varParams[i] = annotate(new Var(param), sps.get(param));
      }
      if (Boolean.TRUE.equals(arrayPair.car()))
      {
        lambda = new Lambda(varParams, compiledBody);
      }
      else
      {
        lambda = new Lambda(Arrays.copyOf(varParams, params.length - 1), compiledBody, varParams[params.length - 1]);
      }
    }
    else
    {
      lambda = new Lambda(new Var[0], compiledBody, new Var((Sym) paramsObj));
    }
    lambda.setProperty("keywordSp", sps.get(pair.car()));
    lambda.setProperty("paramSp", sps.get(paramsObj));
    return annotate(lambda, sps.get(pair));
  }

  private <T extends Node> T annotate(T node, SpData spData)
  {
    node.setProperty("sp", spData);
    return node;
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
    String source = "(lambda\t()\n42)";
    SpParser2 parser = new SpParser2(source);
    Object data = parser.next();
    Map<Object, SpData> sps = parser.getSps();
    Node node = new StremeSpDataCompiler2(sps).compile(data);
    System.out.println(node);
    System.out.println(sps);
    SpData sp = (SpData) node.getProperty("sp");
    System.out.println(sp);
    System.out.println("|" + source.substring(sp.getPos(), sp.getEndPos()) + "|");
  }
}
