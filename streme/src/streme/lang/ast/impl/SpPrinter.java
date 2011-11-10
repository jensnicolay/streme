package streme.lang.ast.impl;

import java.util.IdentityHashMap;
import java.util.Map;

import streme.lang.StremeException;
import streme.lang.ast.Application;
import streme.lang.ast.AstVisitor;
import streme.lang.ast.Begin;
import streme.lang.ast.Binding;
import streme.lang.ast.Define;
import streme.lang.ast.If;
import streme.lang.ast.Lambda;
import streme.lang.ast.Let;
import streme.lang.ast.Literal;
import streme.lang.ast.Literal.Kind;
import streme.lang.ast.Node;
import streme.lang.ast.Ref;
import streme.lang.ast.SetVar;
import streme.lang.ast.Var;
import streme.lang.data.Data;
import streme.lang.data.Null;
import streme.lang.data.Pair;
import streme.lang.data.SpData;
import streme.lang.data.SpParser2;

public class SpPrinter extends AstVisitor
{
  public static String print(Node node)
  {
    SpPrinter printer = new SpPrinter();
    node.accept(printer);
    return printer.toString();
  }

  private StringBuilder sb;

  public SpPrinter()
  {
    super();
    sb = new StringBuilder();
  }

  protected StringBuilder getStringBuilder()
  {
    return sb;
  }

  private void introducePrefix(Node node, String property)
  {
    SpData sp = (SpData) node.getProperty(property);
    if (sp == null)
    {
      sb.append(" ");
      return;
    }
    String prefix = sp.getPrefix();
    if (prefix != null)
    {
      sb.append(prefix);
    }
    else
    {
      throw new StremeException("no prefix available for " + sp + " in " + node);
    }
  }

  private void introduceSuffix(Node node, String property)
  {
    SpData sp = (SpData) node.getProperty(property);
    if (sp == null)
    {
      return;
    }
    String suffix = sp.getSuffix();
    if (suffix != null)
    {
      sb.append(suffix);
    }
    else
    {
      throw new StremeException("no suffix available for " + sp + " in " + node);
    }
  }

  public boolean visitApplication(Application application)
  {
    introducePrefix(application, "sp");
    sb.append("(");
    return true;
  }

  public void endVisitApplication(Application application)
  {
    introduceSuffix(application, "sp");
    sb.append(")");
  }

  public void visitRef(Ref ref)
  {
    introducePrefix(ref, "sp");
    sb.append(ref);
  }

  public void visitLiteral(Literal literal)
  {
    introducePrefix(literal, "sp");
    Object value = literal.getValue();
    if (value == null)
    {
      sb.append("'<undefined>");
    }
    else if (value == void.class)
    {
      sb.append("'<unspecified>");
    }
    else
    {
      Kind kind = literal.getKind();
      switch (kind)
      {
        case CONSTANT:
        {
          sb.append(Data.toString(value));
          break;
        }
        case DATUM:
        {
          sb.append('\'');
          spPrint(value, (Map<Object, SpData>) literal.getProperty("localSps"));
          break;
        }
        case QUOTE:
        {
          sb.append("(quote ").append(Data.toString(value)).append(')');
          break;
        }
        default:
          throw new StremeException("cannot handle " + literal);
      }
    }
  }

  public void visitVar(Var var)
  {
    introducePrefix(var, "sp");
    sb.append(Data.toString(var));
  }

  private void spPrint(Object value, Map<Object, SpData> sps)
  {
    if (value instanceof Pair)
    {
      Map<Pair, Integer> ags = new IdentityHashMap<Pair, Integer>();
      printPair((Pair) value, ags, sps);
    }
    else
    {
      sb.append(Data.toString(value));
    }
  }

  private void printPair(Pair pp, Map<Pair, Integer> ags, Map<Object, SpData> sps)
  {
    Pair p = pp;
    sb.append("(");
    ags.put(p, ags.size());
    do
    {
      Object car = p.car();
      if (car instanceof Pair)
      {
        Integer result = ags.get(car);
        if (result == null)
        {
          SpData spData = sps.get(car);
          sb.append(spData.getPrefix());
          printPair((Pair) car, ags, sps);
        }
        else
        {
          sb.append("°" + result + "°");
        }
      }
      else
      {
        SpData spData = sps.get(car);
        sb.append(spData.getPrefix());
        sb.append(Data.toString(car));
      }
      Object cdr = p.cdr();
      if (cdr instanceof Pair)
      {
        Integer result = ags.get(cdr);
        if (result == null)
        {
          p = (Pair) cdr;
          ags.put(p, ags.size());
          // sb.append(" ");
          continue;
        }
        else
        {
          sb.append(" . °" + result + "°");
          break;
        }
      }
      else if (cdr instanceof Null)
      {
        sb.append(sps.get(pp).getSuffix());
        break;
      }
      else
      {
        sb.append(" . ");
        sb.append(Data.toString(cdr));
        break;
      }
    }
    while (true);
    sb.append(")");
  }

  public boolean visitDefine(Define define)
  {
    introducePrefix(define, "sp");
    sb.append("(");
    introducePrefix(define, "keywordSp");
    sb.append("define");
    return true;
  }

  public void endVisitDefine(Define define)
  {
    introduceSuffix(define, "sp");
    sb.append(")");
  }

  public boolean visitSetVar(SetVar setVar)
  {
    introducePrefix(setVar, "sp");
    sb.append("(");
    introducePrefix(setVar, "keywordSp");
    sb.append("set!");
    return true;
  }

  public void endVisitSetVar(SetVar setVar)
  {
    introduceSuffix(setVar, "sp");
    sb.append(")");
  }

  public boolean visitBegin(Begin begin)
  {
    if (begin.getKind() == Begin.Kind.EXPLICIT)
    {
      introducePrefix(begin, "sp");
      sb.append("(");
      introducePrefix(begin, "keywordSp");
      sb.append("begin");
    }
    return true;
  }

  public void endVisitBegin(Begin begin)
  {
    if (begin.getKind() == Begin.Kind.EXPLICIT)
    {
      introduceSuffix(begin, "sp");
      sb.append(")");
    }
  }

  public boolean visitIf(If ff)
  {
    introducePrefix(ff, "sp");
    sb.append("(");
    introducePrefix(ff, "keywordSp");
    sb.append("if");
    return true;
  }

  public void endVisitIf(If ff)
  {
    introduceSuffix(ff, "sp");
    sb.append(")");
  }

  public boolean visitLambda(Lambda lambda)
  {
    introducePrefix(lambda, "sp");
    sb.append("(");
    introducePrefix(lambda, "keywordSp");
    sb.append("lambda");
    introducePrefix(lambda, "paramSp");
    sb.append("(");
    for (Var var : lambda.getParams())
    {
      var.accept(this);
    }
    introduceSuffix(lambda, "paramSp");
    sb.append(")");
    lambda.getBody().accept(this);
    introduceSuffix(lambda, "sp");
    sb.append(")");
    return false;
  }

  public boolean visitLet(Let let)
  {
    introducePrefix(let, "sp");
    sb.append("(");
    introducePrefix(let, "keywordSp");
    sb.append("let");
    introducePrefix(let, "bindingsSp");
    sb.append("(");
    for (Binding binding : let.getBindings())
    {
      binding.accept(this);
    }
    introduceSuffix(let, "bindingsSp");
    sb.append(")");
    let.getBody().accept(this);
    introduceSuffix(let, "sp");
    sb.append(")");
    return false;
  }

  public boolean visitBinding(Binding binding)
  {
    introducePrefix(binding, "sp");
    sb.append("(");
    return true;
  }

  public void endVisitBinding(Binding binding)
  {
    introduceSuffix(binding, "sp");
    sb.append(")");
  }

  public String toString()
  {
    return sb.toString();
  }

  public static void main(String[] args)
  {
    String source = "(define writez\n    (lambda ()\n        (set! z 123)))";
    SpParser2 parser = new SpParser2(source);
    StremeSpDataCompiler2 compiler = new StremeSpDataCompiler2(parser.getSps());
    Node ast = compiler.compile(parser.next());
    String result = SpPrinter.print(ast);
    System.out.println(source);
    System.out.println(result);
    System.out.println(parser.getSps());
  }
}
