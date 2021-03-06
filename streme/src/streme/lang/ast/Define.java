package streme.lang.ast;

import java.util.Arrays;
import java.util.List;

import streme.lang.data.Lst;
import streme.lang.data.Sym;

public class Define extends Node
{

  private Var var;
  private Node value;

  public Define(Var var, Node value)
  {
    super();
    this.var= var;
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
  	if (visitor.visitDefine(this))
  	{
  	  visitChildren(visitor);
  	}
  	visitor.endVisitDefine(this);
  }

  public Lst toData()
  {
    return Lst.valueOf(new Sym("define"), getVar().toData(), getValue().toData());
  }

  public Type type()
  {
    return Type.DEFINE;
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
    if (node.type() != Type.DEFINE)
    {
      return false;
    }
    Define define = (Define) node;
    return getVar().equals(define.getVar())
      && getValue().nodeEquals(define.getValue());
  }

  public Lst children()
  {
    return Lst.valueOf(getVar(), getValue());
  }
  
  public Define fromChildren(Lst nodes)
  {
    return new Define((Var) nodes.car(), (Node) nodes.cadr());
  }
}
