package streme.lang.ast;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import streme.lang.data.Lst;
import streme.lang.data.Pair;

public class Application extends Node
{
  private Node operator;
  private Node[] operands;

  public Application(Node operator, Node... operands)
  {
    super();
    this.operator = operator;
    this.operands = operands;
  }

  public Node getOperator()
  {
    return operator;
  }

  public int numOperands()
  {
    return operands.length;
  }

  public Node[] getOperands()
  {
    return operands;
  }
  
//  public List<Node> operandsList()
//  {
//    return Arrays.asList(operands);
//  }
//
  public void accept(AstVisitor visitor)
  {
    if (visitor.visitApplication(this))
    {
      visitChildren(visitor);
    }
    visitor.endVisitApplication(this);
  }

  public Lst toData()
  {
    int numOperands = operands.length;
    Object[] els = new Object[1 + numOperands];
    els[0] = operator.toData();
    for (int i = 0; i < numOperands; i++)
    {
      els[i + 1] = operands[i].toData();
    }
    return Pair.valueOf(els);
  }
  
  public Type type()
  {
    return Type.APPLICATION;
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
    if (node.type() != Type.APPLICATION)
    {
      return false;
    }
    Application application = (Application) node;
    return getOperator().nodeEquals(application.getOperator()) 
      && nodeEquals(getOperands(), application.getOperands());
  }
  
  public void setOperator(Node operator)
  {
    this.operator = operator;
  }
  
  public void setOperands(Node[] operands)
  {
    this.operands = operands;
  }
  
  public Lst children()
  {
    return Pair.cons(getOperator(), Lst.valueOf(operands));
  }
  
  public Application fromChildren(Lst children)
  {
    Node operator = (Node) children.car();
    Node[] operands = ((Lst) children.cdr()).properToArray(Node.class);
    return new Application(operator, operands);
  }
  
}
