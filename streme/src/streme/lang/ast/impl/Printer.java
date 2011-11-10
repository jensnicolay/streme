package streme.lang.ast.impl;

import streme.lang.ast.Application;
import streme.lang.ast.AstVisitor;
import streme.lang.ast.Begin;
import streme.lang.ast.Binding;
import streme.lang.ast.Define;
import streme.lang.ast.Future;
import streme.lang.ast.If;
import streme.lang.ast.Lambda;
import streme.lang.ast.Let;
import streme.lang.ast.Literal;
import streme.lang.ast.Node;
import streme.lang.ast.SetVar;
import streme.lang.ast.Var;
import streme.lang.data.Lst;
import streme.lang.data.Parser2;
import streme.lang.data.Sym;

public class Printer extends AstVisitor
{

  public static String print(Node node, String... meta)
  {
    Printer printer = new Printer(meta);
    node.accept(printer);
    return printer.toString();
  }
  
  private StringBuilder sb;
  private String[] meta;
  private int indent;

  public Printer(String... meta)
  {
    super();
    sb = new StringBuilder();
    this.meta = meta;
  }
  
  protected StringBuilder getStringBuilder()
  {
    return sb;
  }
  
  private void indent()
  {
    for (int i = 0; i < indent; i++)
    {
      sb.append(' ');
    }
  }  
  
  private void visitMeta(Node node)
  {
    boolean hasMeta = false;
    for (String s : meta)
    {
      Object value = node.getProperty(s);
      if (value != null)
      {
        if (!hasMeta)
        {
          sb.append("^(");
          hasMeta = true;
          sb.append("(").append(s).append(" . ").append(value).append(")");
        }
        else
        {
          sb.append(" (").append(s).append(" . ").append(value).append(")");
        }
      }
    }
    if (hasMeta)
    {
      sb.append(") ");
    }
  }

  public boolean visitNode(Node node)
  {
    visitMeta(node);
    sb.append(node);
    return false;
  }
  
  public void visitLiteral(Literal literal)
  {
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
      sb.append(value);
    }
  }

  public boolean visitBegin(Begin begin)
  {
    sb.append("(begin");
    indent++;
    for (Node node : begin.getExps())
    {
      sb.append('\n');
      indent();
      node.accept(this);
    }
    sb.append(")");
    indent--;
    return false;
  }

  public boolean visitDefine(Define define)
  {
    sb.append("(define ");
    define.getVar().accept(this);
    sb.append(" ");
    define.getValue().accept(this);
    sb.append(")");
    return false;
  }

  public boolean visitBinding(Binding binding)
  {
    sb.append("(");
    binding.getVar().accept(this);
    sb.append(" ");
    binding.getValue().accept(this);
    sb.append(")");
    return false;
  }

  public boolean visitFuture(Future future)
  {
    sb.append("(future ");
    future.getValue().accept(this);
    sb.append(")");
    return false;
  }

  public boolean visitIf(If ff)
  {
    sb.append("(if ");
    ff.getCondition().accept(this);
    sb.append(" ");
    ff.getConsequent().accept(this);
    sb.append(" ");
    ff.getAlternate().accept(this);
    sb.append(")");
    return false;
  }

  public boolean visitLambda(Lambda lambda)
  {
    visitMeta(lambda);
    sb.append("(lambda (");
    for (Var param : lambda.getParams())
    {
      param.accept(this);
      sb.append(" ");
    }
    if (lambda.getVarparam() != null)
    {
      sb.append(". ");
      lambda.getVarparam().accept(this);
    }
    sb.append(") ");
    lambda.getBody().accept(this);
    sb.append(")");
    return false;
  }

  public boolean visitLet(Let let)
  {
    sb.append("(").append(let.getKind()).append(" (");
    Binding[] bindings = let.getBindings();
    if (bindings.length > 0)
    {
      bindings[0].accept(this);
      for (int i = 1; i < bindings.length; i++)
      {
        sb.append(" ");
        bindings[i].accept(this);
      }
    }
    sb.append(") ");
    let.getBody().accept(this);
    sb.append(")");
    return false;
  }

  public boolean visitSetVar(SetVar setVar)
  {
    visitMeta(setVar);
    sb.append("(set! ");
    setVar.getVar().accept(this);
    sb.append(" ");
    setVar.getValue().accept(this);
    sb.append(")");
    return false;
  }
  
  public boolean visitApplication(Application application)
  {
    sb.append("(");
    application.getOperator().accept(this);
    for (Node operand : application.getOperands())
    {
      sb.append(" ");
      operand.accept(this);
    }
    sb.append(")");
    return false;
  }
  
  public String toString()
  {
    return sb.toString();
  }
  
  public static void main(String[] args)
  {
    Parser2 parser = new Parser2();
    StremeDataCompiler compiler = new StremeDataCompiler();
    String source = "(letrec ((fib (lambda (n)  (if (< n 2) n (+ ('fib (- n 1)) (fib (- n 2))))))) '(fib 2))";
    Node ast = compiler.compile(parser.parse(source));
    Printer tp = new Printer();
    ast.accept(tp);
    String result = tp.toString();
    System.out.println(result);
  }
}
