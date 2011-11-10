package streme.lang.ast.impl;

import java.lang.reflect.Array;

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

public class RecursiveDescentAstRewriter implements AstRewriter
{
  
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
    return node;
  }
  
  protected Node rewriteRef(Ref node)
  {
    return node;
  }

  protected Node rewriteSetVar(SetVar node)
  {
    return new SetVar((Var) rewrite(node.getVar()), rewrite(node.getValue()));
  }

  protected Node rewriteLiteral(Literal node)
  {
    return node;
  }

  protected Node rewriteLet(Let node)
  {
    return new Let(node.getKind(), rewriteBindings(node.getBindings()), rewrite(node.getBody()));
  }

  protected Binding[] rewriteBindings(Binding[] bindings)
  {
    Binding[] result = new Binding[bindings.length];
    for (int i = 0; i < bindings.length; i++)
    {
      result[i] = rewriteBinding(bindings[i]);
    }
    return result;
  }
  
  protected Binding rewriteBinding(Binding binding)
  {
    return new Binding((Var) rewrite(binding.getVar()), rewrite(binding.getValue()));
  }

  protected Node rewriteLambda(Lambda node)
  {
    return new Lambda(rewrite(node.getParams(), Var.class), rewrite(node.getBody()), node.getVarparam() == null ? null : (Var) rewrite(node.getVarparam()));
  }

  protected Node rewriteIf(If node)
  {
    return new If(rewrite(node.getCondition()), rewrite(node.getConsequent()), rewrite(node.getAlternate()));
  }

  protected Node rewriteFuture(Future node)
  {
    return new Future(rewrite(node.getValue()));
  }

  protected Node rewriteDefine(Define node)
  {
    return new Define((Var) rewrite(node.getVar()), rewrite(node.getValue()));
  }

  protected Node rewriteBegin(Begin node)
  {
    return new Begin(rewrite(node.getExps(), Node.class), node.getKind());
  }

  protected Node rewriteApplication(Application node)
  {
    return new Application(rewrite(node.getOperator()), rewrite(node.getOperands(), Node.class));
  }

  protected <T> T[] rewrite(Node[] operands, Class<T> c)
  {
    T[] rewritten = (T[]) Array.newInstance(c, operands.length);
    for (int i = 0; i < operands.length; i++)
    {
      rewritten[i] = (T) rewrite(operands[i]);
    }
    return rewritten;
  }
}
