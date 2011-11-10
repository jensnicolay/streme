package streme.lang.ast.impl;

import streme.lang.ast.Application;
import streme.lang.ast.Lambda;
import streme.lang.ast.Node;
import streme.lang.ast.Var;
import streme.lang.ast.analysis.AnfConverter;
import streme.lang.ast.analysis.RenamingStrategy;
import streme.lang.data.Parser2;

public class TagPrinter extends Printer
{
  
  public static String print(Node node)
  {
    Printer printer = new TagPrinter();
    node.accept(printer);
    return printer.toString();
  }
  
  public boolean visitApplication(Application application)
  {
    StringBuilder sb = getStringBuilder();
    sb.append("(");
    application.getOperator().accept(this);
    sb.append("$_{").append(application.getTag()).append("}$");
    for (Node operand : application.getOperands())
    {
      sb.append(" ");
      operand.accept(this);
    }
    sb.append(")");
    return false;
  }

  public boolean visitLambda(Lambda lambda)
  {
    StringBuilder sb = getStringBuilder();
    sb.append("(lambda");
    sb.append("$_{").append(lambda.getTag()).append("}$");
    sb.append(" (");
    for (Var param : lambda.getParams())
    {
      sb.append(" ");
      param.accept(this);
    }
    sb.append(") ");
    lambda.getBody().accept(this);
    sb.append(")");
    return false;
  }

  
  public static void main(String[] args)
  {
    Parser2 parser = new Parser2();
    StremeDataCompiler compiler = new StremeDataCompiler();
    String source = "(letrec ((fib (lambda (n)  (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))))) (fib 2))";
    Node p = compiler.compile(parser.parse(source));
    AnfConverter ac = new AnfConverter(RenamingStrategy.NUMBER_RENAMING_STRATEGY);
    Node ast = ac.rewrite(p);
    TagPrinter tp = new TagPrinter();
    ast.accept(tp);
    System.out.println(tp.toString());
  }
}
