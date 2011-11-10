package streme.lang.ast;

import java.util.Arrays;
import java.util.List;

import streme.lang.data.IData;
import streme.lang.data.Lst;

public class Binding extends Node implements IData
{
  // public static boolean bindingsEquals(Binding[] b1s, Binding[] b2s)
  // {
  // int length = b1s.length;
  // if (b2s.length != length)
  // {
  // return false;
  // }
  // for (int i = 0; i < length; i++)
  // {
  // if (!b1s[i].getVar().nodeEquals(b2s[i].getVar()) || !b1s[i].getValue().nodeEquals(b2s[i].getValue()))
  // {
  // return false;
  // }
  // }
  // return true;
  // }
  private Var var;
  private Node value;

  public Binding(Var var, Node value)
  {
    super();
    this.var = var;
    this.value = value;
  }

  public int compareTo(Binding o)
  {
    int cname = var.compareTo(o.var);
    if (cname != 0)
    {
      return cname;
    }
    return value.compareTo(o.value);
  }

  public Var getVar()
  {
    return var;
  }

  public Node getValue()
  {
    return value;
  }
  
  public void setValue(Node value)
  {
    this.value = value;
  }

  public Object toData()
  {
    return Lst.valueOf(var.getName(), value == null ? null : value.toData());
  }

  public void accept(AstVisitor visitor)
  {
    if (visitor.visitBinding(this))
    {
      visitChildren(visitor);
    }
    visitor.endVisitBinding(this);
  }

  public Lst children()
  {
    return Lst.valueOf(var, value);
  }
  
  public Binding fromChildren(Lst nodes)
  {
    return new Binding((Var) nodes.car(), (Node) nodes.cadr());
  }
  
  public Type type()
  {
    return Type.BINDING;
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
    if (node.type() != Type.BINDING)
    {
      return false;
    }
    Binding binding = (Binding) node;
    return getVar().equals(binding.getVar())
      && getValue().nodeEquals(binding.getValue());
  }
}
