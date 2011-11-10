package streme.lang.ast;

import streme.lang.data.Lst;
import streme.lang.data.Null;
import streme.lang.data.Sym;

public class Ref extends Node
{
  private Sym name;
  
  public Ref(String name)
  {
    this(new Sym(name));
  }
  
  public Ref(Sym name)
  {
    super();
    this.name = name;
  }

  public Sym getName()
  {
    return name;
  }
  
  public Sym toData()
  {
    return name;
  }
  
  public Type type()
  {
    return Type.REF;
  }
  
  public void accept(AstVisitor visitor)
  {
    visitor.visitRef(this);
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
    if (node.type() != Type.REF)
    {
      return false;
    }
    return ((Ref) node).getName().equals(getName());
  }
  
  public Lst children()
  {
    return new Null();
  }
  
  public Ref fromChildren(Lst nodes)
  {
    return new Ref(name);
  }
}
