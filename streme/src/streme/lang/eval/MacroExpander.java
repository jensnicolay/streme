package streme.lang.eval;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import streme.lang.StremeException;
import streme.lang.ast.AstDataCompiler;
import streme.lang.ast.analysis.RenamingStrategy;
import streme.lang.ast.impl.StremeDataCompiler;
import streme.lang.data.DataRewriter;
import streme.lang.data.DataUnifier;
import streme.lang.data.Lst;
import streme.lang.data.Null;
import streme.lang.data.Pair;
import streme.lang.data.Parser2;
import streme.lang.data.Sym;

public class MacroExpander implements DataRewriter
{
  private static final Object UNDEFINED = null;
  private static final RenamingStrategy renamer = RenamingStrategy.NUMBER_RENAMING_STRATEGY;
  private boolean rewriteLet;
  private boolean rewriteLetrec;
  private Set<Pattern> patterns;
  private Lst macros;
  private DataUnifier unifier;
  private DataEvaluator dataEvaluator;

  public MacroExpander()
  {
    this(null);
  }

  public MacroExpander(DataEvaluator dataEvaluator)
  {
    super();
    rewriteLet = true;
    rewriteLetrec = true;
    patterns = new LinkedHashSet<Pattern>();
    macros = new Null();
    unifier = new DataUnifier();
    this.dataEvaluator = dataEvaluator;
  }

  public void setLetToLambda(boolean rewriteLet)
  {
    this.rewriteLet = rewriteLet;
  }

  public void setLetrecToLambda(boolean rewriteLetrec)
  {
    this.rewriteLetrec = rewriteLetrec;
  }

  public Object rewrite(Object data)
  {
    if (data instanceof Pair)
    {
      Pair p = (Pair) data;
      Object car = p.car();
      if (car instanceof Sym)
      {
        Sym sym = (Sym) car;
        String name = sym.getName();
        if ("quote".equals(name))
        {
          return data;
        }
        if ("define".equals(name))
        {
          return rewriteDefine((Pair) p.cdr());
        }
        if ("lambda".equals(name))
        {
          return rewriteLambda((Pair) p.cdr());
        }
        if ("let".equals(name))
        {
          return rewriteLet((Pair) p.cdr());
        }
        if ("let||".equals(name))
        {
          return rewriteLetPar((Pair) p.cdr());
        }
        if ("let*".equals(name))
        {
          return rewriteLetStar((Pair) p.cdr());
        }
        if ("letrec".equals(name))
        {
          return rewriteLetrec((Pair) p.cdr());
        }
        if ("begin".equals(name))
        {
          return rewriteBegin((Lst) p.cdr());
        }
        if ("and".equals(name))
        {
          return rewriteAnd((Lst) p.cdr());
        }
        if ("or".equals(name))
        {
          return rewriteOr((Lst) p.cdr());
        }
        if ("cond".equals(name))
        {
          return rewriteCond((Lst) p.cdr());
        }
        if ("case".equals(name))
        {
          return rewriteCase((Pair) p.cdr());
        }
        if ("do".equals(name))
        {
          return rewriteDo((Pair) p.cdr());
        }
        if ("future".equals(name))
        {
          return rewriteFuture((Pair) p.cdr());
        }
        if ("quasiquote".equals(name))
        {
          return rewriteQuasiQuote((Pair) p.cdr());
        }
        if ("define-pattern".equals(name))
        {
          return rewriteDefinePattern((Pair) p.cdr());
        }
        if ("define-macro".equals(name))
        {
          return rewriteDefineMacro((Pair) p.cdr());
        }
      }
      for (Pattern pattern : patterns)
      {
        Map<Sym, Object> subs = unifier.unify(data, pattern.getPattern());
        if (subs != null)
        {
          Object rewritten = unifier.apply(subs, pattern.getRewrite(), true);
          return rewrite(rewritten);
        }
      }
      if (car instanceof Sym)
      {
        for (Object macroObj : macros)
        {
          Macro macro = (Macro) macroObj;
          if (car.equals(macro.getName()))
          {
            int args = (int) ((Lst) p.cdr()).length();
            if (macro.getArgs().length == args || (macro.getArgs().length < args && macro.getVararg() != null))
            {
              Map<Sym, Object> subs = new HashMap<Sym, Object>();
              Lst l = (Lst) p.cdr();
              for (int i = 0; i < macro.getArgs().length; i++)
              {
                subs.put(macro.getArgs()[i], l.car());
                l = (Lst) l.cdr();
              }
              if (macro.getVararg() != null)
              {
                subs.put(macro.getVararg(), l);
              }
              Object unified = unifier.apply(subs, macro.getRewrite(), true);
              Object evaluated = dataEvaluator.evaluateData(unified, dataEvaluator.globalEnv());
              return rewrite(evaluated);
            }
          }
        }
        String sym = car.toString();
        int slashIndex = sym.indexOf("/");
        if (slashIndex > 0)
        {
          Lst args = (Lst) p.cdr();
          String className = sym.substring(0, slashIndex);
          String[] members = sym.substring(slashIndex + 1).split("\\.");
          {
            Lst l = Lst
                .valueOf(new Sym("invoke-static"), Lst.valueOf(new Sym("class-for-name"), className), members[0]);
            for (int i = 1; i < members.length; i++)
            {
              l = Lst.valueOf(new Sym("invoke"), l, members[i]);
            }
            return rewrite(l.append(args));
          }
        }
        int dotIndex = sym.indexOf(".");
        if (dotIndex == 0)
        {
          Lst args = (Lst) p.cddr();
          return rewrite(Pair.cons(new Sym("invoke"), Pair.cons(p.cadr(), (Pair.cons(sym.substring(1), args)))));
        }
        if (sym.endsWith("."))
        {
          Lst args = (Lst) p.cdr();
          return rewrite(Pair.cons(new Sym("new"),
              Pair.cons(Lst.valueOf(new Sym("class-for-name"), sym.substring(0, sym.length() - 1)), args)));
        }
      }
      Pair array = p.toArray();
      Object[] a = (Object[]) array.cdr();
      for (int i = 0; i < a.length; i++)
      {
        a[i] = rewrite(a[i]);
      }
      if ((Boolean) array.car())
      {
        return Lst.valueOf(a);
      }
      else
      {
        return Lst.valueOfImproper(a);
      }
    }
    // only match with meta-vars in patterns, but not from source
    boolean isMetaVar = data instanceof Sym && ((Sym) data).getName().startsWith("?");
    if (data != null && !isMetaVar)
    {
      for (Pattern pattern : patterns)
      {
        Map<Sym, Object> subs = unifier.unify(data, pattern.getPattern());
        if (subs != null)
        {
          Object rewritten = unifier.apply(subs, pattern.getRewrite(), true);
          return rewrite(rewritten);
        }
      }
    }
    if (data instanceof Sym)
    {
      String sym = data.toString();
      int slashIndex = sym.indexOf('/');
      if (slashIndex > 0)
      {
        String className = sym.substring(0, slashIndex);
        String[] members = sym.substring(slashIndex + 1).split("\\.");
        {
          Lst l = Lst.valueOf(new Sym("invoke-static"), Lst.valueOf(new Sym("class-for-name"), className), members[0]);
          for (int i = 1; i < members.length; i++)
          {
            l = Lst.valueOf(new Sym("invoke"), l, members[i]);
          }
          return rewrite(l);
        }
      }
    }
    return data;
  }

  private Object rewriteFuture(Pair future)
  {
    return Pair.cons(new Sym("future"), Pair.cons(rewrite(future.car()), new Null()));
  }

  private Object rewriteLambda(Pair lambda)
  {
    return Pair.cons(new Sym("lambda"), Pair.cons(lambda.car(), rewriteBody((Lst) lambda.cdr())));
  }

  private Object rewriteDefine(Pair define)
  {
    Object idObj = define.car();
    Pair value = (Pair) define.cdr();
    if (idObj instanceof Pair)
    {
      Pair idPair = (Pair) idObj;
      Sym id = (Sym) idPair.car();
      Object args = idPair.cdr();
      return Lst.valueOf(new Sym("define"), id, rewrite(Pair.cons(new Sym("lambda"), Pair.cons(args, value))));
    }
    else
    {
      Object rewrittenValue = rewrite(value);
      return Pair.cons(new Sym("define"), Pair.cons(idObj, rewrittenValue));
    }
  }

  private Object rewriteCond(Lst clauses)
  {
    if (clauses.isNull())
    {
      return UNDEFINED;
    }
    Lst clause = (Lst) clauses.car();
    Object test = clause.car();
    if (clause.cdr() instanceof Null)
    {
      return rewrite(test);
    }
    if ("else".equals(Sym.getName(test)))
    {
      if (clause.cddr() instanceof Null)
      {
        return rewrite(clause.cadr());
      }
      else
      {
        return rewrite(Pair.cons(new Sym("begin"), clause.cdr()));
      }
    }
    else
    {
      if (clause.cddr() instanceof Null)
      {
        return rewrite(Lst.valueOf(new Sym("if"), test, clause.cadr(), Pair.cons(new Sym("cond"), clauses.cdr())));
      }
      else
      {
        return rewrite(Lst.valueOf(new Sym("if"), test, Pair.cons(new Sym("begin"), clause.cdr()), Pair.cons(new Sym("cond"), clauses.cdr())));
      }
    }
  }

  private Object rewriteCase(Pair pair)
  {
    Object value = pair.car();
    Lst clauses = (Lst) pair.cdr();
    List<Object> rewrittenClauses = new ArrayList<Object>();
    Sym temp = renamer.rename(new Sym("t"));
    for (Object clause : clauses)
    {
      Pair c = (Pair) clause;
      if ("else".equals(Sym.getName(c.car())))
      {
        rewrittenClauses.add(c);
      }
      else
      {
        rewrittenClauses.add(Pair.cons(Lst.valueOf(new Sym("memv"), temp, Lst.valueOf(new Sym("quote"), c.car())), c.cdr()));
      }
    }
    return rewrite(Lst.valueOf(new Sym("let"), Lst.valueOf(Lst.valueOf(temp, value)),
        Pair.cons(new Sym("cond"), Lst.valueOf(rewrittenClauses))));
  }

  private Object rewriteDo(Pair pair)
  {
    Lst varInitSteps = (Lst) pair.car();
    List<Object> vars = new ArrayList<Object>();
    List<Object> inits = new ArrayList<Object>();
    List<Object> steps = new ArrayList<Object>();
    for (Object varInitStepObj : varInitSteps)
    {
      Pair varInitStep = (Pair) varInitStepObj;
      vars.add(varInitStep.car());
      inits.add(varInitStep.cadr());
      if (varInitStep.cddr() instanceof Null)
      {
        steps.add(varInitStep.car());
      }
      else
      {
        steps.add(varInitStep.caddr());
      }
    }
    Lst testExps = (Lst) pair.cadr();
    Object test = testExps.car();
    Lst exps = (Lst) testExps.cdr();
    Object consequent = (exps.isNull() ? null : exps.cdr() instanceof Null ? exps.car() : Pair.cons(new Sym("begin"), exps));
    Sym loopName = renamer.rename(new Sym("loop"));
    Object loop = Pair.cons(loopName, Lst.valueOf(steps));
    Lst body = (Lst) pair.cddr();
    Object alternate;
    if (body.isNull())
    {
      alternate = loop;
    }
    else
    {
      alternate = Pair.cons(new Sym("begin"), body.append(Pair.cons(loop, new Null())));
    }
    Lst letrec = Lst.valueOf(
        new Sym("letrec"),
        Lst.valueOf(Lst.valueOf(loopName,
            Lst.valueOf(new Sym("lambda"), Lst.valueOf(vars), Lst.valueOf(new Sym("if"), test, consequent, alternate)))),
        Pair.cons(loopName, Lst.valueOf(inits)));
    return rewrite(letrec);
  }

  private Object rewriteAnd(Lst lst)
  {
    if (lst.isNull())
    {
      return new Boolean(true);
    }
    Lst rest = (Lst) lst.cdr();
    if (rest.isNull())
    {
      return rewrite(lst.car());
    }
    return rewrite(Lst.valueOf(new Sym("if"), lst.car(), Pair.cons(new Sym("and"), lst.cdr()), new Boolean(false)));
  }

  private Object rewriteOr(Lst lst)
  {
    if (lst.isNull())
    {
      return new Boolean(false);
    }
    Lst rest = (Lst) lst.cdr();
    if (rest.isNull())
    {
      return rewrite(lst.car());
    }
    Sym temp = renamer.rename(new Sym("t"));
    return rewrite(Lst.valueOf(new Sym("let"), Lst.valueOf(Lst.valueOf(temp, lst.car())),
        Lst.valueOf(new Sym("if"), temp, temp, Pair.cons(new Sym("or"), lst.cdr()))));
  }

  private Object rewriteLetStar(Pair pair)
  {
    Lst bindings = (Lst) pair.car();
    Pair body = (Pair) pair.cdr();
    if (bindings.isNull())
    {
      return rewrite(Pair.cons(new Sym("let"), Pair.cons(new Null(), body)));
    }
    Object firstBinding = bindings.car();
    Object restBindings = bindings.cdr();
    if (restBindings instanceof Null)
    {
      return rewrite(Pair.cons(new Sym("let"), Pair.cons(bindings, body)));
    }
    else
    {
      return rewrite(Pair.cons(new Sym("let"),
          Lst.valueOf(Pair.cons(firstBinding, new Null()), Pair.cons(new Sym("let*"), Pair.cons(restBindings, body)))));
    }
  }

  private Object rewriteLet(Pair pair)
  {
    if (pair.car() instanceof Sym)
    {
      Sym name = (Sym) pair.car();
      Lst bindings = (Lst) pair.cadr();
      Pair body = (Pair) pair.cddr();
      List<Object> params = new ArrayList<Object>();
      List<Object> operands = new ArrayList<Object>();
      for (Object bindingObj : bindings)
      {
        Pair binding = (Pair) bindingObj;
        params.add(binding.car());
        operands.add(binding.cadr());
      }
      Lst r = Lst.valueOf(new Sym("letrec"),
          Lst.valueOf(Lst.valueOf(name, Pair.cons(new Sym("lambda"), Pair.cons(Lst.valueOf(params), body)))),
          Pair.cons(name, Lst.valueOf(operands)));
      return rewrite(r);
    }
    else
    {
      Lst bindings = (Lst) pair.car();
      if (pair.cdr() instanceof Null)
      {
        throw new StremeException("no body for let with bindings " + bindings);
      }
      Pair body = (Pair) pair.cdr();
      if (rewriteLet)
      {
        List<Object> params = new ArrayList<Object>();
        List<Object> operands = new ArrayList<Object>();
        for (Object bindingObj : bindings)
        {
          Pair binding = (Pair) bindingObj;
          params.add(binding.car());
          operands.add(binding.cadr());
        }
        return rewrite(Pair.cons(Pair.cons(new Sym("lambda"), Pair.cons(Lst.valueOf(params), body)), Lst.valueOf(operands)));
      }
      List<Object> rbindings = new ArrayList<Object>();
      for (Object bindingObj : bindings)
      {
        Pair binding = (Pair) bindingObj;
        rbindings.add(Pair.cons(binding.car(), Pair.cons(rewrite(binding.cadr()), new Null())));
      }
      return Pair.cons(new Sym("let"), Pair.cons(Lst.valueOf(rbindings), rewrite(body)));
    }
  }

  private Object rewriteLetPar(Pair pair)
  {
    Lst bindings = (Lst) pair.car();
    Pair body = (Pair) pair.cdr();
    List<Object> parbindings = new ArrayList<Object>();
    List<Object> touches = new ArrayList<Object>();
    for (Object bindingObj : bindings)
    {
      Pair binding = (Pair) bindingObj;
      parbindings.add(Lst.valueOf(binding.car(), Pair.cons(new Sym("future"), binding.cdr())));
      touches.add(Lst.valueOf(new Sym("set!"), binding.car(), Pair.cons(new Sym("touch"), Pair.cons(binding.car(), new Null()))));
    }
    return rewrite(Lst.valueOf(new Sym("let"), Lst.valueOf(parbindings)).append(Lst.valueOf(touches)).append(body));
  }

  private Object rewriteLetrec(Pair pair)
  {
    Lst bindings = (Lst) pair.car();
    Pair body = (Pair) pair.cdr();
    if (rewriteLetrec)
    {
      List<Object> nullBindings = new ArrayList<Object>();
      List<Object> setters = new ArrayList<Object>();
      for (Object bindingObj : bindings)
      {
        Pair binding = (Pair) bindingObj;
        nullBindings.add(Pair.cons(binding.car(), Pair.cons(null, new Null())));
        setters.add(Pair.cons(new Sym("set!"), Pair.cons(binding.car(), binding.cdr())));
      }
      Pair cons = Pair.cons(new Sym("let"), Pair.cons(Lst.valueOf(nullBindings), Lst.valueOf(setters).append(body)));
      return rewrite(cons);
    }
    List<Object> rbindings = new ArrayList<Object>();
    for (Object bindingObj : bindings)
    {
      Pair binding = (Pair) bindingObj;
      rbindings.add(Pair.cons(binding.car(), Pair.cons(rewrite(binding.cadr()), new Null())));
    }
    return Pair.cons(new Sym("letrec"), Pair.cons(Lst.valueOf(rbindings), rewrite(body)));
  }

  private Object rewriteBegin(Lst body)
  {
    List<Object> rewritten = new ArrayList<Object>();
    for (Object op : body)
    {
      rewritten.add(rewrite(op));
    }
    return Pair.cons(new Sym("begin"), Lst.valueOf(rewritten));
  }

  private Object rewriteQuasiQuote(Lst body)
  {
    return rewriteQuasiQuoteElement(body.car());
  }

  private Object rewriteQuasiQuoteElement(Object object)
  {
    if (object instanceof Pair)
    {
      Pair p = (Pair) object;
      if ("unquote".equals(Sym.getName(p.car())))
      {
        return rewrite(p.cadr());
      }
      else
      {
        return rewriteQuasiQuoteList(p);
      }
    }
    else if (object instanceof Null)
    {
      return new Null();
    }
    else
    {
      return Pair.cons(new Sym("quote"), Pair.cons(object, new Null()));
    }
  }

  private Lst rewriteQuasiQuoteList(Pair p)
  {
    List<Object> els = new ArrayList<Object>();
    while (true)
    {
      Object car = p.car();
      if (car instanceof Pair && "unquote-splicing".equals(Sym.getName(((Pair) car).car())))
      {
        Pair<Sym, Lst> l1 = Pair.cons(new Sym("list"), Lst.valueOf(els));
        Object l2 = ((Pair) car).cadr();
        Object l3 = p.cdr() instanceof Null ? new Null() : Pair.cons(new Sym("list"),
            Pair.cons(rewrite(Pair.cons(new Sym("quasiquote"), p.cdr())), new Null()));
        return Lst.valueOf(new Sym("append"), l1, l2, l3);
      }
      else
      {
        Object rewritten = rewriteQuasiQuoteElement(car);
        els.add(rewritten);
      }
      Object cdr = p.cdr();
      if (cdr instanceof Pair)
      {
        p = (Pair) cdr;
        continue;
      }
      if (cdr instanceof Null)
      {
        return Pair.cons(new Sym("list"), Lst.valueOf(els));
      }
      els.add(rewriteQuasiQuoteElement(cdr));
      return Pair.cons(new Sym("improper-list"), Lst.valueOf(els));
    }
  }

  private Lst rewriteBody(Lst body)
  {
    List<Object> internalDefineLambdaBindings = new ArrayList<Object>();
    List<Object> internalDefineValuesBindings = new ArrayList<Object>();
    List<Object> operations = new ArrayList<Object>();
    for (Object operand : body)
    {
      if (operand instanceof Pair && "define".equals(Sym.getName(((Pair) operand).car())))
      {
        Pair pair = (Pair) ((Pair) operand).cdr();
        Object idObj = pair.car();
        Lst definedValuePair = (Lst) pair.cdr();
        if (idObj instanceof Sym)
        {
          Object definedValueCar = definedValuePair.car();
          if (definedValueCar instanceof Pair && "lambda".equals(Sym.getName(((Pair) definedValueCar).car())))
          {
            internalDefineLambdaBindings.add(Pair.cons(idObj, definedValuePair));
          }
          else
          {
            internalDefineValuesBindings.add(Pair.cons(idObj, definedValuePair));
          }
        }
        else if (idObj instanceof Pair)
        {
          Pair idPair = (Pair) idObj;
          Sym id = (Sym) idPair.car();
          Object args = idPair.cdr();
          Pair defBody = (Pair) definedValuePair;
          internalDefineLambdaBindings.add(Pair.cons(id,
              Pair.cons(Pair.cons(new Sym("lambda"), Pair.cons(args, defBody)), new Null())));
        }
        else
        {
          throw new StremeException("define: illegal syntax");
        }
      }
      else
      {
        operations.add(operand);
      }
    }
    if (operations.isEmpty())
    {
      throw new StremeException("body: no expressions in body");
    }
    List<Object> rewrittenOperations = new ArrayList<Object>();
    for (int i = 0; i < operations.size(); i++)
    {
      Object operation = operations.get(i);
      rewrittenOperations.add(rewrite(operation));
    }
    if (internalDefineLambdaBindings.isEmpty())
    {
      if (internalDefineValuesBindings.isEmpty())
      {
        return Lst.valueOf(rewrittenOperations);
      }
      else
      {
        return Lst.valueOf(rewrite(Pair.cons(new Sym("let*"),
            Pair.cons(Lst.valueOf(internalDefineValuesBindings), Lst.valueOf(rewrittenOperations)))));
      }
    }
    else
    {
      if (internalDefineValuesBindings.isEmpty())
      {
        return Lst.valueOf(rewrite(Pair.cons(new Sym("letrec"),
            Pair.cons(Lst.valueOf(internalDefineLambdaBindings), Lst.valueOf(rewrittenOperations)))));
      }
      else
      {
        return Lst.valueOf(rewrite(Pair.cons(
            new Sym("let*"),
            Pair.cons(
                Lst.valueOf(internalDefineValuesBindings),
                Lst.valueOf(rewrite(Pair.cons(new Sym("letrec"),
                    Pair.cons(Lst.valueOf(internalDefineLambdaBindings), Lst.valueOf(rewrittenOperations)))))))));
      }
    }
  }

  private static class Pattern
  {
    private Object pattern;
    private Object rewrite;

    public Pattern(Object pattern, Object rewrite)
    {
      super();
      this.pattern = pattern;
      this.rewrite = rewrite;
    }

    public int hashCode()
    {
      final int prime = 31;
      int result = 1;
      result = prime * result + ((pattern == null) ? 0 : pattern.hashCode());
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
      final Pattern other = (Pattern) obj;
      if (pattern == null)
      {
        if (other.pattern != null)
          return false;
      }
      else if (!pattern.equals(other.pattern))
        return false;
      return true;
    }

    public Object getPattern()
    {
      return pattern;
    }

    public Object getRewrite()
    {
      return rewrite;
    }

    public String toString()
    {
      return "<pattern " + pattern + ">";
    }
  }

  private static class Macro
  {
    private Sym name;
    private Sym[] args;
    private Sym vararg;
    private Object rewrite;

    public Macro(Sym name, Sym[] args, Sym vararg, Object rewrite)
    {
      super();
      this.name = name;
      this.args = args;
      this.vararg = vararg;
      this.rewrite = rewrite;
    }

    public int hashCode()
    {
      final int prime = 31;
      int result = 1;
      result = prime * result + Arrays.hashCode(args);
      result = prime * result + ((name == null) ? 0 : name.hashCode());
      result = prime * result + ((vararg == null) ? 0 : vararg.hashCode());
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
      final Macro other = (Macro) obj;
      if (!Arrays.equals(args, other.args))
        return false;
      if (name == null)
      {
        if (other.name != null)
          return false;
      }
      else if (!name.equals(other.name))
        return false;
      if (vararg == null)
      {
        if (other.vararg != null)
          return false;
      }
      else if (!vararg.equals(other.vararg))
        return false;
      return true;
    }

    public Sym getName()
    {
      return name;
    }

    public Sym[] getArgs()
    {
      return args;
    }

    public Sym getVararg()
    {
      return vararg;
    }

    public Object getRewrite()
    {
      return rewrite;
    }

    public String toString()
    {
      return "<macro " + name + ">";
    }
  }

  private Object rewriteDefinePattern(Pair pair)
  {
    Object pattern = pair.car();
    Lst rewrite = (Lst) pair.cdr();
    Object actualRewrite;
    if (rewrite.length() > 1)
    {
      actualRewrite = Pair.cons(new Sym("begin"), rewrite);
    }
    else
    {
      actualRewrite = rewrite.car();
    }
    Pattern p = new Pattern(pattern, actualRewrite);
    patterns.add(p);
    return p;
  }

  private Object rewriteDefineMacro(Pair pair)
  {
    Lst rewrite = (Lst) pair.cdr();
    Object actualRewrite;
    if (rewrite.length() > 1)
    {
      actualRewrite = Pair.cons(new Sym("begin"), rewrite);
    }
    else
    {
      actualRewrite = rewrite.car();
    }
    Lst sig = (Lst) pair.car();
    Sym name = (Sym) sig.car();
    Object ps = sig.cdr();
    Macro macro;
    if (ps instanceof Lst)
    {
      Pair<Boolean, Sym[]> params = ((Lst) ps).toArray(Sym.class);
      if (params.car())
      {
        macro = new Macro(name, params.cdr(), null, actualRewrite);
      }
      else
      {
        Sym varparam = params.cdr()[params.cdr().length - 1];
        macro = new Macro(name, Arrays.copyOfRange(params.cdr(), 0, params.cdr().length - 1), varparam, actualRewrite);
      }
    }
    else
    {
      Sym varparam = (Sym) ps;
      macro = new Macro(name, new Sym[0], varparam, actualRewrite);
    }
    macros = Pair.cons(macro, macros);
    return macro;
  }

  public static void main(String[] args)
  {
    Parser2 parser = new Parser2();
    MacroExpander macroExpander = new MacroExpander();
    macroExpander.setLetToLambda(false);
    macroExpander.setLetrecToLambda(false);
    Object source = parser.parse("((lambda () (begin 42 (define x 1)) x))");
    System.out.println(source);
    Object expanded = macroExpander.rewrite(source);
    System.out.println(expanded);
    AstDataCompiler compiler = new StremeDataCompiler();
    System.out.println(compiler.compile(source));
  }
}
