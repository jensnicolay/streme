package streme.lang.ast;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import streme.lang.data.Lst;
import streme.lang.data.Null;
import streme.lang.data.Pair;
import streme.lang.data.Sym;

public class Lambda extends Node
{
  private Var[] params;
  private Var varparam;
  private Node body;

  public Lambda(Var[] params, Node body)
  {
    this(params, body, null);
  }

  public Lambda(Var[] params, Node body, Var varparam)
  {
    super();
    this.params = params;
    this.body = body;
    this.varparam = varparam;
  }

  public Var[] getParams()
  {
    return params;
  }
  
  public List<Var> paramsList()
  {
    return Arrays.asList(params);
  }

  public Var getVarparam()
  {
    return varparam;
  }

  public Node getBody()
  {
    return body;
  }

  public void setBody(Node body)
  {
    this.body = body;
  }

  public boolean isVararg()
  {
    return varparam != null;
  }

  public Lambda addFirstParam(Var param)
  {
    Var[] newParams = new Var[params.length + 1];
    System.arraycopy(params, 0, newParams, 1, params.length);
    newParams[0] = param;
    return new Lambda(newParams, body);
  }

  public void accept(AstVisitor visitor)
  {
    if (visitor.visitLambda(this))
    {
      visitChildren(visitor);
    }
    visitor.endVisitLambda(this);
  }

  public String toShortString()
  {
    StringBuilder sb = new StringBuilder("(lambda (");
    int numParams = getParams().length;
    if (numParams > 0)
    {
      int i;
      for (i = 0; i < numParams - 1; i++)
      {
        sb.append(getParams()[i]).append(" ");
      }
      sb.append(getParams()[i]);
    }
    sb.append(") ");
    sb.append(shorten(getBody().toShortString()));
    sb.append(")");
    return sb.toString();
  }

  public Lst toData()
  {
    List<Object> els = new ArrayList<Object>();
    els.add(new Sym("lambda"));
    Object ps;
    if (isVararg())
    {
      if (params.length == 0)
      {
        ps = varparam.getName();
      }
      else
      {
        Sym[] dparams = new Sym[params.length + 1];
        int i;
        for (i = 0; i < params.length; i++)
        {
          dparams[i] = params[i].getName();
        }
        dparams[i] = varparam.getName();
        ps = Lst.valueOfImproper(dparams);
      }
    }
    else
    {
      Sym[] dparams = new Sym[params.length];
      int i;
      for (i = 0; i < params.length; i++)
      {
        dparams[i] = params[i].getName();
      }
      ps = Lst.valueOf(dparams);      
    }
    els.add(ps);
    Begin.bodyToData(body, els);
    return Lst.valueOf(els);
  }

  public Type type()
  {
    return Type.LAMBDA;
  }

  public boolean nodeEquals(Node node)
  {
    if (node == null)
    {
      return false;
    }
    if (this == node)
    {
      return true;
    }
    if (node.type() != Type.LAMBDA)
    {
      return false;
    }
    Lambda lambda = (Lambda) node;
    return nodeEquals(getParams(), lambda.getParams()) && getVarparam() == lambda.getVarparam()
        && getBody().nodeEquals(lambda.getBody());
  }
  
  
  public Lst children()
  {
    Lst result = new Null();
    result = Pair.cons(body, result);
    if (varparam != null)
    {
      result = Pair.cons(varparam, result);
    }
    for (int i = params.length - 1; i >= 0; i--)
    {
      result = Pair.cons(params[i], result);
    }
    return result;
  }
  
  public Lambda fromChildren(Lst nodes2)
  {
    Node[] nodes = nodes2.properToArray(Node.class);
    if (varparam == null)
    {
      return new Lambda(Arrays.copyOf(nodes, nodes.length - 1, Var[].class), nodes[nodes.length - 1]);
    }
    return new Lambda(Arrays.copyOf(nodes, nodes.length - 2, Var[].class), nodes[nodes.length - 1], (Var) nodes[nodes.length - 2]);    
  }
}
