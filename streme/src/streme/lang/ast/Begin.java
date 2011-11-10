package streme.lang.ast;

import java.util.Arrays;
import java.util.List;

import streme.lang.ast.Begin.Kind;
import streme.lang.data.Lst;
import streme.lang.data.Sym;

public class Begin extends Node
{
  
  public enum Kind {IMPLICIT, EXPLICIT};
  
  public static void bodyToData(Node body, List<Object> els)
  {
    if (body.type() == Type.BEGIN)
    {
      Node[] exps = ((Begin) body).getExps();
      for (int i = 0; i < exps.length; i++)
      {
        els.add(exps[i].toData());
      }
    }
    else
    {
      els.add(body.toData());
    }
  }
  
  public static Node singleNode(List<Node> nodes, Kind kind)
  {
    if (nodes.size() == 1)
    {
      return nodes.get(0);
    }
    return new Begin(nodes, kind);
  }
  
  private Node[] exps;
  private Kind kind;

  public Begin(Node[] exps, Kind kind)
  {
    super();
    this.exps = exps;
    this.kind = kind;
  }
  
  public Begin(List<Node> exps, Kind kind)
  {
    this(exps.toArray(new Node[exps.size()]), kind);
  }
  
  public Node[] getExps()
  {
	  return exps;
  }
  
  public Kind getKind()
  {
    return kind;
  }
  
  public Node[] getTailExps()
  {
    return Arrays.copyOfRange(exps, 1, exps.length);
  }
  
  public void accept(AstVisitor visitor)
  {
  	if (visitor.visitBegin(this))
  	{
  	  visitChildren(visitor);
  	}
    visitor.endVisitBegin(this);
  }

  public Object toData()
  {
    if (kind == Kind.IMPLICIT)
    {
      return exps;
    }
    int numExps = exps.length;
    Object[] els = new Object[numExps + 1];
    els[0] = new Sym("begin");
    for (int i = 0; i < numExps; i++)
    {
      els[i + 1] = exps[i].toData();
    }
    return Lst.valueOf(els);
  }
  
  public Type type()
  {
    return Type.BEGIN;
  }
  
  public String toShortString()
  {
    return "(begin " + exps[0].toShortString() + (exps.length > 0 ? " ...)" : ")");
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
    if (node.type() != Type.BEGIN)
    {
      return false;
    }
    Begin begin = (Begin) node;
    return Node.nodeEquals(getExps(), begin.getExps());
  }
  
//  public Begin prepend(Node... nodes)
//  {
//    Node[] newExps = Arrays.copyOf(nodes, nodes.length + exps.length);
//    System.arraycopy(exps, 0, newExps, nodes.length, exps.length);
//    return new Begin(newExps);
//  }
  
  public Begin append(Node... nodes)
  {
    Node[] newExps = Arrays.copyOf(exps, exps.length + nodes.length);
    System.arraycopy(nodes, 0, newExps, exps.length, nodes.length);
    return new Begin(newExps, kind);
  }
  
  public Lst children()
  {
    return Lst.valueOf(exps);
  }
  
  public Begin fromChildren(Lst nodes)
  {
    return new Begin(nodes.properToArray(Node.class), kind);
  }
  
}
