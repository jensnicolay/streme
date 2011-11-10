package streme.lang.ast;

import streme.lang.data.Lst;
import streme.lang.data.Null;

public class Literal extends Node
{
  
  public static enum Kind {QUOTE, DATUM, CONSTANT}
  
  public static final Literal UNSPECIFIED = new Literal(Void.TYPE, Kind.CONSTANT);
  public static final Literal UNDEFINED = new Literal(null, Kind.CONSTANT);
  
  private Object value;
  private Kind kind;

  public Literal(Object value, Kind kind)
  {
    super();
    this.value = value;
    this.kind = kind;
  }

  public Object toData()
  {
    return value;
  }

  public Object getValue()
  {
    return value;
  }
  
  public Kind getKind()
  {
    return kind;
  }

  public void accept(AstVisitor visitor)
  {
    visitor.visitLiteral(this);
  }

  public Type type()
  {
    return Type.LITERAL;
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
    if (node.type() != Type.LITERAL)
    {
      return false;
    }
    Object value = ((Literal) node).getValue();
    if (value == this.value)
    {
      return true;
    }
    if (value == null)
    {
      return false;
    }
    return value.equals(this.getValue());
  }

  public Lst children()
  {
    return new Null();
  }
  
  public Literal fromChildren(Lst nodes)
  {
    return new Literal(value, kind);
  }
}
