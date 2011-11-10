package streme.lang.ast;

import streme.lang.data.Lst;
import streme.lang.data.Null;
import streme.lang.data.Pair;
import streme.lang.data.Sym;



public class Future extends Node
{
  
  private Node value;
  
  public Future(Node value)
  {
    super();
    this.value = value;
  }
  
  public Node getValue()
  {
    return value;
  }
  
  public void accept(AstVisitor visitor)
  {
    if (visitor.visitFuture(this))
    {
      visitChildren(visitor);
    }
    visitor.endVisitFuture(this);
  }
  
  public Type type()
  {
    return Type.FUTURE;
  }
  
  public Lst toData()
  {
    return Lst.valueOf(new Sym("future"), getValue().toData());
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
    if (node.type() != Type.FUTURE)
    {
      return false;
    }
    Future future = (Future) node;
    return getValue().nodeEquals(future.getValue());
  }
  
  public Lst children()
  {
    return Pair.cons(value, new Null());
  }
  
  public Future fromChildren(Lst nodes)
  {
    return new Future((Node) nodes.car());
  }
}
