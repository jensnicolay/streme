package streme.lang.ast.impl;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import streme.lang.StremeException;
import streme.lang.ast.Application;
import streme.lang.ast.AstDataCompiler;
import streme.lang.ast.Begin;
import streme.lang.ast.Define;
import streme.lang.ast.If;
import streme.lang.ast.Lambda;
import streme.lang.ast.Node;
import streme.lang.ast.Node.Type;
import streme.lang.ast.Ref;
import streme.lang.ast.SetVar;
import streme.lang.ast.Var;
import streme.lang.data.Lst;
import streme.lang.data.Null;
import streme.lang.data.Pair;
import streme.lang.data.Parser2;
import streme.lang.data.Sym;

public class NodeUnifier
{
  public interface ConditionHandler
  {
    Map<Sym, Node> accept(Sym var, Node node, String condition);
  }
  
  public Map<Sym, Node> unify(Node n1, Node n2, ConditionHandler conditionHandler)
  {
    Object d1 = n1.toData();
    Object d2 = n2.toData();
    if ("?".equals(Sym.getName(d1)) || "?".equals(Sym.getName(d2)))
    {
      return new HashMap<Sym, Node>();
    }
    if (d1 instanceof Sym)
    {
      String name = ((Sym) d1).getName();
      if (name.startsWith("?"))
      {
        Map<Sym, Node> s = new HashMap<Sym, Node>();
        if (name.endsWith("}"))
        {
          String actualName = name.substring(0, name.indexOf('}'));
          String condition = name.substring(name.indexOf('{'), name.length() - 1);
          Map<Sym, Node> ss = conditionHandler.accept(new Sym(actualName), n2, condition);
          if (ss == null)
          {
            return null;
          }
          s.putAll(ss);
        }
        else
        {
          s.put((Sym) d1, n2);
        }
        return s;        
      }
    }
    if (d2 instanceof Sym)
    {
      String name = ((Sym) d2).getName();
      if (name.startsWith("?"))
      {
        Map<Sym, Node> s = new HashMap<Sym, Node>();
        if (name.endsWith("}"))
        {
          String actualName = name.substring(0, name.indexOf('{'));
          String condition = name.substring(name.indexOf('{') + 1, name.length() - 1);
          Map<Sym, Node> ss = conditionHandler.accept(new Sym(actualName), n1, condition);
          if (ss == null)
          {
            return null;
          }
          s.putAll(ss);
        }
        else
        {
          s.put((Sym) d2, n1);
        }
        return s;        
      }
    }
    Type t1 = n1.type();
    Type t2 = n2.type();
    if (t1 != t2)
    {
      return null;
    }
    if (n1.nodeEquals(n2))
    {
      return new HashMap<Sym, Node>();
    }
    switch (t1)
    {
      case REF:
      case LITERAL:
      {
        return null;
      }
      default:
      {
        Lst p1 = n1.children();
        Lst p2 = n2.children();
        return unify2(p1, p2, conditionHandler);
      }
    }
  } 

  private Map<Sym, Node> unify2(Lst p1, Lst p2, ConditionHandler conditionHandler)
  {
    if (p1.isNull() && p2.isNull())
    {
      return new HashMap<Sym, Node>();
    }
    if (p1.isNull() || p2.isNull())
    {
      return null;
    }
    Map<Sym, Node> subs1 = unify((Node) p1.car(), (Node) p2.car(), conditionHandler);
    if (subs1 == null)
    {
      return null;
    }
    Lst cdr1 = (Lst) p1.cdr();
    Lst cdr2 = (Lst) p2.cdr();
    cdr1 = applyLst(subs1, cdr1);
    cdr2 = applyLst(subs1, cdr2);
    Map<Sym, Node> subs2 = unify2(cdr1, cdr2, conditionHandler);
    if (subs2 == null)
    {
      return null;
    }
    subs1.putAll(subs2);
    return subs1;
  }

  public static Lst applyLst(Map<Sym, Node> subs, Lst l)
  {
    if (l.isNull())
    {
      return new Null();
    }
    Node car = (Node) l.car();
    if (car.type() == Type.REF && ((Ref) car).getName().getName().startsWith("?"))
    {
      Node r = subs.get(((Ref) car).getName());
      return Pair.cons(r == null ? applyNode(subs, car) : r, applyLst(subs, (Lst) l.cdr()));
    }
    return Pair.cons(applyNode(subs, car), applyLst(subs, (Lst) l.cdr()));
  }

  public static Node applyNode(Map<Sym, Node> subs, Node car)
  {
    Lst p = car.children();
    if (p.isNull())
    {
      return car;
    }
    Lst t = applyLst(subs, p);
    switch (car.type())
    {
      case APPLICATION:
      {
        return new Application((Node) t.car(), ((Lst) t.cdr()).toArray(Node.class).cdr());
      }
      case DEFINE:
      {
        return new Define((Var) t.car(), (Node) t.cadr());
      }
      case IF:
      {
        return new If((Node) t.car(), (Node) t.cadr(), (Node) t.caddr());
      }
      case BEGIN:
      {
        return new Begin(t.properToArray(Node.class), Begin.Kind.EXPLICIT);
      }
      case SETVAR:
      {
        return new SetVar((Var) t.car(), (Node) t.cadr());
      }
      case LAMBDA:
      {
        Lambda lambda = (Lambda) car;
        int numParams = lambda.getParams().length;
        Var[] params = new Var[numParams];
        for (int i = 0; i < numParams; i++)
        {
          params[i] = (Var) t.car();
          t = (Lst) t.cdr();
        }
        Var varParam;
        if (lambda.getVarparam() != null)
        {
          varParam = (Var) t.car();
          t = (Lst) t.cdr();
        }
        else
        {
          varParam = null;
        }
        Node body = (Node) t.car();
        return new Lambda(params, body, varParam);
      }
      default:
        throw new StremeException("cannot handle " + car);
    }
  }

  public static void main(String[] args)
  {
    String s1 = "(1 2)";
    String s2 = "(?a ?b)";
    Parser2 parser = new Parser2();
    AstDataCompiler compiler = new StremeDataCompiler();
    Node n1 = compiler.compile(parser.parse(s1));
    Node n2 = compiler.compile(parser.parse(s2));
    NodeUnifier nu = new NodeUnifier();
    System.out.println(nu.unify(n1, n2, null));
  }
}
