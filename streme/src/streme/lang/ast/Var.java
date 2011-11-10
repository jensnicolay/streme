package streme.lang.ast;

import streme.lang.data.Lst;
import streme.lang.data.Null;
import streme.lang.data.Sym;

public class Var extends Node
{
  private Sym name;
  
  public Var(String name)
  {
    this(new Sym(name));
  }
  
  public Var(Sym name)
  {
    super();
    this.name = name;
  }

//  public int compareTo(Var var)
//  {
//    return getName().compareTo(var.getName());
//  }

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
    return Type.VAR;
  }
  
  public void accept(AstVisitor visitor)
  {
    visitor.visitVar(this);
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
    if (node.type() != Type.VAR)
    {
      return false;
    }
    return ((Var) node).getName().equals(getName());
  }
  
  public Lst children()
  {
    return new Null();
  }
  
  public Var fromChildren(Lst nodes)
  {
    return new Var(name);
  }
}
