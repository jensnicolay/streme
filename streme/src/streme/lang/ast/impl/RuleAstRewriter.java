package streme.lang.ast.impl;

import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import streme.lang.StremeException;
import streme.lang.ast.Application;
import streme.lang.ast.AstRewriter;
import streme.lang.ast.Begin;
import streme.lang.ast.Binding;
import streme.lang.ast.Define;
import streme.lang.ast.Future;
import streme.lang.ast.If;
import streme.lang.ast.Lambda;
import streme.lang.ast.Let;
import streme.lang.ast.Literal;
import streme.lang.ast.Node;
import streme.lang.ast.Ref;
import streme.lang.ast.SetVar;
import streme.lang.ast.Var;
import streme.lang.data.Pair;
import streme.lang.data.Parser2;

public class RuleAstRewriter implements AstRewriter
{
  public static abstract class Rule
  {
    public abstract boolean matches(Node node);
    public abstract Pair<Boolean, Node> rewrite(Node node);
    protected Pair<Boolean, Node> rewrote(Node node)
    {
      return Pair.cons(true, node);
    }
    protected Pair<Boolean, Node> unchanged(Node node)
    {
      return Pair.cons(false, node);
    }    
  }
  
  private List<Rule> rules;

  public RuleAstRewriter(Rule...rules)
  {
    this(Arrays.asList(rules));
  }

  public RuleAstRewriter(List<Rule> rules)
  {
    super();
    this.rules = rules;
  }

  public Pair<Boolean, Node> ruleRewrite(Node node)
  {
    for (Rule rule : rules)
    {
      if (rule.matches(node))
      {
        return rule.rewrite(node);
      }
    }
    return Pair.cons(false, null);
  }

  public Node rewrite(Node node)
  {
    switch (node.type())
    {
      case APPLICATION:
        return rewriteApplication((Application) node);
      case BEGIN:
        return rewriteBegin((Begin) node);
      case DEFINE:
        return rewriteDefine((Define) node);
      case FUTURE:
        return rewriteFuture((Future) node);
      case IF:
        return rewriteIf((If) node);
      case LAMBDA:
        return rewriteLambda((Lambda) node);
      case LET:
        return rewriteLet((Let) node);
      case LITERAL:
        return rewriteLiteral((Literal) node);
      case SETVAR:
        return rewriteSetVar((SetVar) node);
      case VAR:
        return rewriteVar((Var) node);
      case REF:
        return rewriteRef((Ref) node);
      case BINDING:
        return rewriteBinding((Binding) node);
      default:
        throw new StremeException("cannot handle " + node);
    }
  }

  protected Node rewriteVar(Var node)
  {
    Pair<Boolean, Node> pvar = ruleRewrite(node);
    return pvar.car() ? pvar.cdr() : node;
  }

  protected Node rewriteRef(Ref node)
  {
    Pair<Boolean, Node> pref = ruleRewrite(node);
    return pref.car() ? pref.cdr() : node;
  }

  protected Node rewriteSetVar(SetVar node)
  {
    Pair<Boolean, Node> psetvar = ruleRewrite(node);
    if (psetvar.car())
    {
      return psetvar.cdr();
    }
    Var rvar = (Var) rewriteVar(node.getVar());
    Node rbody = rewrite(node.getValue());
    return new SetVar(rvar, rbody);
  }

  protected Node rewriteLiteral(Literal node)
  {
    Pair<Boolean, Node> pliteral = ruleRewrite(node);
    return pliteral.car() ? pliteral.cdr() : node;
  }

  protected Node rewriteLet(Let node)
  {
    Pair<Boolean, Node> plet = ruleRewrite(node);
    if (plet.car())
    {
      return plet.cdr();
    }
    Binding[] rbindings = rewriteBindings(node.getBindings());
    Node rbody = rewrite(node.getBody());
    if (rbindings == null)
    {
      return rbody;
    }
    return new Let(node.getKind(), rbindings, rbody);
  }

  protected Binding[] rewriteBindings(Binding[] bindings)
  {
    List<Binding> rbindings = new ArrayList<Binding>();
    for (int i = 0; i < bindings.length; i++)
    {
      Binding rbinding = rewriteBinding(bindings[i]);
      if (rbinding != null)
      {
        rbindings.add(rbinding);
      }
    }
    if (rbindings.isEmpty())
    {
      return null;
    }
    return rbindings.toArray(new Binding[rbindings.size()]);
  }

  protected Binding rewriteBinding(Binding binding)
  {
    Pair<Boolean, Node> pbinding = ruleRewrite(binding);
    if (pbinding.car())
    {
      return (Binding) pbinding.cdr();
    }
    Var rvar = (Var) rewrite(binding.getVar());
    if (rvar == null)
    {
      return null;
    }
    Node rvalue = rewrite(binding.getValue());
    if (rvalue == null)
    {
      return null;
    }
    return new Binding(rvar, rvalue);
  }

  protected Node rewriteLambda(Lambda node)
  {
    Pair<Boolean, Node> plambda = ruleRewrite(node);
    if (plambda.car())
    {
      return plambda.cdr();
    }
    Var[] rparams = rewrite(node.getParams(), Var.class);
    Var rvarparam = node.getVarparam() == null ? null : (Var) rewrite(node.getVarparam());
    return new Lambda(rparams, rewrite(node.getBody()), rvarparam);
  }

  protected Node rewriteIf(If node)
  {
    Pair<Boolean, Node> pif = ruleRewrite(node);
    if (pif.car())
    {
      return pif.cdr();
    }
    return new If(rewrite(node.getCondition()), rewrite(node.getConsequent()), rewrite(node.getAlternate()));
  }

  protected Node rewriteFuture(Future node)
  {
    Pair<Boolean, Node> pfuture = ruleRewrite(node);
    if (pfuture.car())
    {
      return pfuture.cdr();
    }
    return new Future(rewrite(node.getValue()));
  }

  protected Node rewriteDefine(Define node)
  {
    Pair<Boolean, Node> pdefine = ruleRewrite(node);
    if (pdefine.car())
    {
      return pdefine.cdr();
    }
    return new Define((Var) rewrite(node.getVar()), rewrite(node.getValue()));
  }

  protected Node rewriteBegin(Begin node)
  {
    Pair<Boolean, Node> pbegin = ruleRewrite(node);
    if (pbegin.car())
    {
      return pbegin.cdr();
    }
    Node[] rexps = rewrite(node.getExps(), Node.class);
    if (rexps.length == 0)
    {
      return null;
    }
    return new Begin(rexps, node.getKind());
  }

  protected Node rewriteApplication(Application node)
  {
    Pair<Boolean, Node> papplication = ruleRewrite(node);
    if (papplication.car())
    {
      return papplication.cdr();
    }
    return new Application(rewrite(node.getOperator()), rewrite(node.getOperands(), Node.class));
  }

  protected <T> T[] rewrite(Node[] operands, Class<T> c)
  {
    List<T> rewritten = new ArrayList<T>();
    for (int i = 0; i < operands.length; i++)
    {
      T r = (T) rewrite(operands[i]);
      if (r != null)
      {
        rewritten.add(r);
      }
    }
    return rewritten.toArray((T[]) Array.newInstance(c, rewritten.size()));
  }

  public static void main(String[] args)
  {
    String source = "";
    Parser2 parser = new Parser2();
    StremeDataCompiler compiler = new StremeDataCompiler();
    List<Rule> rules = new ArrayList<Rule>();
    rules.add(new Rule()
    {
      public boolean matches(Node node)
      {
        return (node instanceof SetVar);
      }

      public Pair<Boolean, Node> rewrite(Node node)
      {
        return Pair.cons(true, null);
      }
    });
  }
}
