package streme.lang.ast;

import streme.lang.data.Lst;
import streme.lang.data.Null;
import streme.lang.data.Sym;



public class If extends Node
{
  private Node condition;
  private Node consequent;
  private Node alternate;

  public If(Node condition, Node consequent, Node alternate)
  {
    super();
    this.condition = condition;
    this.consequent = consequent;
    this.alternate = alternate;
  }

  public Node getCondition()
  {
    return condition;
  }

  public Node getConsequent()
  {
    return consequent;
  }

  public Node getAlternate()
  {
    return alternate;
  }
  
  public void accept(AstVisitor visitor)
  {
    if (visitor.visitIf(this))
    {
      visitChildren(visitor);
    }
    visitor.endVisitIf(this);
  }

  public Lst toData()
  {
    return Lst.valueOf(new Sym("if"), getCondition().toData(), getConsequent().toData(), getAlternate().toData());
  }
  
  public String toShortString()
  {
    return shorten("(if " + getCondition().toShortString() + " " + getConsequent().toShortString() + " " + getAlternate().toShortString() + ")");
  }
  
  public Type type()
  {
    return Type.IF;
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
    if (node.type() != Type.IF)
    {
      return false;
    }
    If iff = (If) node;
    return getCondition().nodeEquals(iff.getCondition())
      && getConsequent().nodeEquals(iff.getConsequent())
      && getAlternate().nodeEquals(iff.getAlternate());
  }
  
  
  public Lst children()
  {
    return Lst.valueOf(getCondition(), getConsequent(), getAlternate());
  }
  
  public If fromChildren(Lst nodes)
  {
    return new If((Node) nodes.car(), (Node) nodes.cadr(), nodes.cddr() instanceof Null ? Literal.UNSPECIFIED : (Node) nodes.caddr());
  }
  
}
