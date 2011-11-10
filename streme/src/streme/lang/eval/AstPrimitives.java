package streme.lang.eval;

import streme.lang.ast.Application;
import streme.lang.ast.Binding;
import streme.lang.ast.Node;
import streme.lang.ast.Node.Type;
import streme.lang.ast.Ref;
import streme.lang.ast.Var;
import streme.lang.data.Lst;
import streme.lang.data.Sym;

public class AstPrimitives
{
  public static boolean varp(Object operand)
  {
    return (operand instanceof Node) && ((Node) operand).type() == Type.VAR;
  }

  public static boolean setvarp(Object operand)
  {
    return (operand instanceof Node) && ((Node) operand).type() == Type.SETVAR;
  }

  public static boolean refp(Object operand)
  {
    return (operand instanceof Node) && ((Node) operand).type() == Type.REF;
  }

  public static boolean lambdap(Object operand)
  {
    return (operand instanceof Node) && ((Node) operand).type() == Type.LAMBDA;
  }

  public static boolean letp(Object operand)
  {
    return (operand instanceof Node) && ((Node) operand).type() == Type.LET;
  }

  public static boolean applicationp(Object operand)
  {
    return (operand instanceof Node) && ((Node) operand).type() == Type.APPLICATION;
  }

  public static boolean ifp(Object operand)
  {
    return (operand instanceof Node) && ((Node) operand).type() == Type.IF;
  }

  public static boolean literalp(Object operand)
  {
    return (operand instanceof Node) && ((Node) operand).type() == Type.LITERAL;
  }

  public static boolean beginp(Object operand)
  {
    return (operand instanceof Node) && ((Node) operand).type() == Type.BEGIN;
  }

  public static boolean definep(Object operand)
  {
    return (operand instanceof Node) && ((Node) operand).type() == Type.DEFINE;
  }

  public static boolean bindingp(Object operand)
  {
    return (operand instanceof Node) && ((Node) operand).type() == Type.BINDING;
  }

  public static Sym varName(Object operand)
  {
    return ((Var) operand).getName();
  }

  public static Sym refName(Object operand)
  {
    return ((Ref) operand).getName();
  }
  
  public static Var bindingVar(Object operand)
  {
    return ((Binding) operand).getVar();
  }

  public static Node bindingValue(Object operand)
  {
    return ((Binding) operand).getValue();
  }
  
  public static Node applicationOperator(Object operand)
  {
    return ((Application) operand).getOperator();
  }
  
  public static Lst applicationOperands(Object operand)
  {
    return Lst.valueOf(((Application) operand).getOperands());
  }
  
  public static Node applicationOperand(Object operand, Object index)
  {
    return ((Application) operand).getOperands()[(Integer) index];
  }
  
  public static boolean nameEqualp(Object operand1, Object operand2)
  {
    return operand1.toString().equals(operand2.toString());
  }

  private AstPrimitives()
  {
    super();
  }
}
