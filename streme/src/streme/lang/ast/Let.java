package streme.lang.ast;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import streme.lang.data.Lst;
import streme.lang.data.Null;
import streme.lang.data.Pair;
import streme.lang.data.Sym;

public class Let extends Node
{
  public enum Kind
  {
    LET("let"), LETSTAR("let*"), LETREC("letrec"), LETPAR("let||");
    private Sym sym;

    Kind(String kind)
    {
      sym = new Sym(kind);
    }

    public Sym getKind()
    {
      return sym;
    }

    public String toString()
    {
      return sym.toString();
    }
  }
  
  private Kind kind;
  private Binding[] bindings;
  private Node body;

  public Let(Kind kind, Var name, Node value, Node body)
  {
    this(kind, new Binding[] { new Binding(name, value)}, body);
  }

  public Let(Kind kind, Var name1, Node value1, Var name2, Node value2, Node body)
  {
    this(kind, new Binding[] { new Binding(name1, value1), new Binding(name2, value2)}, body);
  }
  
  public Let(Kind kind, List<Binding> bindings, Node body)
  {
    this(kind, bindings.toArray(new Binding[bindings.size()]), body);
  }

  public Let(Kind kind, Binding binding, Node body)
  {
    this(kind, new Binding[] {binding}, body);
  }

  public Let(Kind kind, Binding[] bindings, Node body)
  {
    super();
    this.kind = kind;
    this.bindings = bindings;
    this.body = body;
  }

  public Kind getKind()
  {
    return kind;
  }

  public Binding[] getBindings()
  {
    return bindings;
  }
  
  public List<Binding> bindingsList()
  {
    return Arrays.asList(bindings);
  }

  public Var getName(int i)
  {
    return bindings[i].getVar();
  }

  public Node getValue(int i)
  {
    return bindings[i].getValue();
  }

  public Node getBody()
  {
    return body;
  }

  public Type type()
  {
    return Type.LET;
  }

  public void accept(AstVisitor visitor)
  {
    if (visitor.visitLet(this))
    {
      visitChildren(visitor);
    }
    visitor.endVisitLet(this);
  }

  public Lst toData()
  {
    List<Object> bindingDatas = new ArrayList<Object>();
    for (Binding binding : bindings)
    {
      bindingDatas.add(binding.toData());
    }
    List<Object> els = new ArrayList<Object>();
    els.add(kind.getKind());
    els.add(Lst.valueOf(bindingDatas));
    Begin.bodyToData(body, els);
    return Lst.valueOf(els);
  }

  public String toShortString()
  {
    if (bindings.length > 0)
    {
      return shorten("(" + getKind() + " ((" + getBindings()[0].getVar() + " ... )) " + getBody().toShortString() + ")");
    }
    return shorten("(" + getKind() + " () " + getBody().toShortString() + ")");
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
    if (node.type() != Type.LET)
    {
      return false;
    }
    Let let = (Let) node;
    if (getKind() != let.getKind())
    {
      return false;
    }
    return nodeEquals(getBindings(), let.getBindings()) && getBody().nodeEquals(let.getBody());
  }
  
  /**
   * A "simple" let is of the form <code>
   * (let ((name value)) body)
   * </code>
   */
  public boolean isSimpleLet()
  {
    return getKind() == Kind.LET && getBindings().length == 1;
  }

  public Lst children()
  {
    Lst result = new Null();
    result = Pair.cons(body, result);
    for (int i = bindings.length - 1; i >=0; i--)
    {
      result = Pair.cons(bindings[i], result);
    }
    return result;
  }
 
  public Let fromChildren(Lst nodes2)
  {
    Node[] nodes = nodes2.properToArray(Node.class);
    return new Let(kind, Arrays.copyOf(nodes, nodes.length - 1, Binding[].class), nodes[nodes.length - 1]);
  }
}
