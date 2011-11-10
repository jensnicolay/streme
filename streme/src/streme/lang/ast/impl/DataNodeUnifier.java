package streme.lang.ast.impl;

import java.util.HashMap;
import java.util.Map;

import streme.lang.StremeException;
import streme.lang.ast.Application;
import streme.lang.ast.AstDataCompiler;
import streme.lang.ast.AstVisitor;
import streme.lang.ast.Begin;
import streme.lang.ast.Binding;
import streme.lang.ast.Define;
import streme.lang.ast.Lambda;
import streme.lang.ast.Let;
import streme.lang.ast.Node;
import streme.lang.ast.SetVar;
import streme.lang.data.DataUnifier;
import streme.lang.data.Lst;
import streme.lang.data.Null;
import streme.lang.data.Pair;
import streme.lang.data.Parser2;
import streme.lang.data.Sym;

public class DataNodeUnifier
{
  public interface ConditionHandler
  {
    Map<Sym, Node> accept(Sym var, Node node, String condition);
  }

  private DataUnifier dataUnifier = new DataUnifier();

  public Map<Sym, Node> unify(Object data, Node node, ConditionHandler handler)
  {
    if (isAny(data))
    {
      return new HashMap<Sym, Node>();
    }
    if (data instanceof Sym)
    {
      String name = ((Sym) data).getName();
      if (name.startsWith("?"))
      {
        Map<Sym, Node> s = new HashMap<Sym, Node>();
        if (name.endsWith("}"))
        {
          String actualName = name.substring(0, name.indexOf('{'));
          String condition = name.substring(name.indexOf('{') + 1, name.length() - 1);
          Map<Sym, Node> ss = handler.accept(new Sym(actualName), node, condition);
          if (ss == null)
          {
            return null;
          }
          s.putAll(ss);
        }
        else
        {
          s.put((Sym) data, node);
        }
        return s;
      }
    }
    Object nodeData = node.toData();
    if (data.equals(nodeData))
    {
      return new HashMap<Sym, Node>();
    }
    switch (node.type())
    {
      case LITERAL:
      case REF:
      case VAR:
        return null;
      case DEFINE:
      {
        Define define = (Define) node;
        if (!(data instanceof Pair))
        {
          return null;
        }
        Pair p = (Pair) data;
        if (p.length() != 3)
        {
          return null;
        }
        if (!"define".equals(Sym.getName(p.car())))
        {
          return null;
        }
        Lst l = Lst.valueOf(define.children());
        return unify2((Lst) p.cdr(), l, handler);
      }
      case APPLICATION:
      {
        Application application = (Application) node;
        if (!(data instanceof Lst))
        {
          return null;
        }
        Lst p = (Lst) data;
        Lst l = Lst.valueOf(application.children());
        return unify2(p, l, handler);
      }
      case BEGIN:
      {
        Begin begin = (Begin) node;
        if (!(data instanceof Pair))
        {
          return null;
        }
        Pair p = (Pair) data;
        if (!"begin".equals(Sym.getName(p.car())))
        {
          return null;
        }
        Lst l = Lst.valueOf(begin.children());
        return unify2((Lst) p.cdr(), l, handler);
      }
      case LAMBDA:
      {
        Lambda lambda = (Lambda) node;
        if (!(data instanceof Pair))
        {
          return null;
        }
        Pair p = (Pair) data;
        if (!"lambda".equals(Sym.getName(p.car())))
        {
          return null;
        }
        Lst dataChildren = new Null();
        Object params = p.cadr();
        while (!(params instanceof Null))
        {
          if (params instanceof Pair)
          {
            dataChildren = Pair.cons(((Pair) params).car(), dataChildren);
            params = ((Pair) params).cdr();
          }
          else
          {
            if (lambda.getVarparam() == null)
            {
              return null;
            }
            dataChildren = Pair.cons(((Pair) params).car(), dataChildren);     
            params = new Null();
          }
        }
        dataChildren = dataChildren.append((Lst) p.cddr());
        Lst l = Lst.valueOf(lambda.children());
        return unify2(dataChildren, l, handler);        
      }
      case SETVAR:
      {
        SetVar setVar = (SetVar) node;
        if (!(data instanceof Pair))
        {
          return null;
        }
        Pair p = (Pair) data;
        if ("set!".equals(Sym.getName(p.car())))
        {
          return null;
        }
        Lst l = Lst.valueOf(setVar.children());
        return unify2((Lst) p.cdr(), l, handler);        
      }
      case LET:
      {
        Let let = (Let) node;
        if (!(data instanceof Pair))
        {
          return null;
        }
        Pair p = (Pair) data;
        Sym letSym;
        switch (let.getKind())
        {
          case LET: letSym = new Sym("let"); break;
          case LETSTAR : letSym = new Sym("let*"); break;
          case LETREC: letSym = new Sym("letrec"); break;
          case LETPAR: letSym = new Sym("let||"); break;
          default: throw new StremeException("cannot handle " + let);
        }
        if (!letSym.equals(p.car()))
        {
          return null;
        }
        Lst dataBindings = (Lst) p.cadr();
        Lst body = (Lst) p.cddr();
        Lst dataChildren = dataBindings.append(body);
        Lst nodeChildren = Lst.valueOf(let.children());
        return unify2(dataChildren, nodeChildren, handler);
      }
      case BINDING:
      {
        Binding binding = (Binding) node;
        if (!(data instanceof Pair))
        {
          return null;
        }
        Pair p = (Pair) data;
        if (p.length() != 2)
        {
          return null;
        }
        Lst nodeChildren = Lst.valueOf(binding.children());
        return unify2(p, nodeChildren, handler);
      }
      default:
        throw new StremeException("cannot handle " + node);
    }
  }

  private Map<Sym, Node> unify2(Lst datas1, Lst nodes2, ConditionHandler conditionHandler)
  {
    if (datas1.isNull() && nodes2.isNull())
    {
      return new HashMap<Sym, Node>();
    }
    if (datas1.isNull() || nodes2.isNull())
    {
      return null;
    }
    Map<Sym, Node> subs1 = unify(datas1.car(), (Node) nodes2.car(), conditionHandler);
    if (subs1 == null)
    {
      return null;
    }
    Lst cdrDatas1 = (Lst) datas1.cdr();
    Lst cdrNodes2 = (Lst) nodes2.cdr();
    cdrDatas1 = (Lst) dataUnifier.apply(subs1, cdrDatas1);
    Map<Sym, Node> subs2 = unify2(cdrDatas1, cdrNodes2, conditionHandler);
    if (subs2 == null)
    {
      return null;
    }
    subs1.putAll(subs2);
    return subs1;
  }

  private boolean isAny(Object object)
  {
    return object instanceof Sym && ((Sym) object).getName().equals("?");
  }

  public static void main(String[] args)
  {
    String source = "(define x x)";
    final Parser2 parser = new Parser2();
    AstDataCompiler compiler = new StremeDataCompiler();
    Node ast = compiler.compile(parser.parse(source));
    final Object query = parser.parse("(define ?x{x} ?)");
    final DataNodeUnifier unifier = new DataNodeUnifier();
    final ConditionHandler conditionHandler = new ConditionHandler()
    {
      public Map<Sym, Node> accept(Sym var, Node node, String condition)
      {
        Object q = parser.parse(condition);
        Map<Sym, Node> s = unifier.unify(q, node, this);
        if (s == null)
        {
          return null;
        }
        s.put(var, node);
        return s;
      }
    };
    ast.accept(new AstVisitor()
    {
      public boolean visitNode(Node node)
      {
        Map<Sym, Node> s = unifier.unify(query, node, conditionHandler);
        if (s != null)
        {
          System.out.println(node + " (" + node.type() + "): " + s);
        }
        return true;
      }
    });
    System.out.println("=========================");
    final NodeUnifier nodeUnifier = new NodeUnifier();
    final Node nodeQuery = compiler.compile(query);
    final NodeUnifier.ConditionHandler conditionHandler2 = new NodeUnifier.ConditionHandler()
    {
      public Map<Sym, Node> accept(Sym var, Node node, String condition)
      {
        Node n = new StremeDataCompiler().compile(parser.parse(condition));
        Map<Sym, Node> s = nodeUnifier.unify(node, n, this);
        if (s == null)
        {
          return null;
        }
        s.put(var, node);
        return s;
      }
    };
    ast.accept(new AstVisitor()
    {
      public boolean visitNode(Node node)
      {
        Map<Sym, Node> s = nodeUnifier.unify(nodeQuery, node, conditionHandler2);
        if (s != null)
        {
          System.out.println(node + " (" + node.type() + "): " + s);
        }
        return true;
      }
    });
  }
}
