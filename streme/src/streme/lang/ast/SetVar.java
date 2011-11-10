package streme.lang.ast;

import java.util.Arrays;
import java.util.List;

import streme.lang.data.Lst;
import streme.lang.data.Sym;

public class SetVar extends Node
{
  private Var var;
  private Node value;

  public SetVar(Var var, Node value)
  {
    super();
    this.var = var;
    this.value = value;
  }

  public Var getVar()
  {
    return var;
  }

  public Node getValue()
  {
    return value;
  }

  public void accept(AstVisitor visitor)
  {
    if (visitor.visitSetVar(this))
    {
      visitChildren(visitor);
    }
    visitor.endVisitSetVar(this);
  }

  public Lst toData()
  {
    return Lst.valueOf(new Sym("set!"), getVar().toData(), getValue().toData());
  }

  public Type type()
  {
    return Type.SETVAR;
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
    if (node.type() != Type.SETVAR)
    {
      return false;
    }
    SetVar setVar = (SetVar) node;
    return getVar().equals(setVar.getVar()) && getValue().nodeEquals(setVar.getValue());
  }

  public Lst children()
  {
    return Lst.valueOf(getVar(), getValue());
  }
  
  public SetVar fromChildren(Lst nodes)
  {
    return new SetVar((Var) nodes.car(), (Node) nodes.cadr());
  }
}
