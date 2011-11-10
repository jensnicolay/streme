package streme.lang.ast.analysis;

import java.util.ArrayList;
import java.util.List;

import streme.lang.StremeException;
import streme.lang.ast.AstDataCompiler;
import streme.lang.ast.Begin;
import streme.lang.ast.Binding;
import streme.lang.ast.Define;
import streme.lang.ast.Let;
import streme.lang.ast.Literal;
import streme.lang.ast.Node;
import streme.lang.ast.SetVar;
import streme.lang.ast.Var;
import streme.lang.ast.impl.Printer;
import streme.lang.ast.impl.RecursiveDescentAstRewriter;
import streme.lang.ast.impl.StremeDataCompiler;
import streme.lang.data.Lst;
import streme.lang.data.Null;
import streme.lang.data.Pair;
import streme.lang.data.Parser2;
import streme.lang.eval.MacroExpander;

public class Undefiner extends RecursiveDescentAstRewriter
{
  
  public static Node undefine(Node node)
  {
    return new Undefiner().rewrite(node);
  }

  protected Node rewriteBegin(Begin begin)
  {
    Node[] exps = begin.getExps();
    Lst bindings = new Null();
    List<Node> operations = new ArrayList<Node>();
    for (int i = 0; i < exps.length; i++)
    {
      Node exp = exps[i];
      if (exp.type() == Node.Type.DEFINE)
      {
        Define define = (Define) exp;
        Node value = define.getValue();
        if (true /*value.type() == Node.Type.LAMBDA*/)
        {
          bindings = Pair.cons(new Binding(new Var(define.getVar().getName()), Literal.UNDEFINED), bindings);
          operations.add(new SetVar(new Var(define.getVar().getName()), rewrite(value)));
        }
        else
        {
          bindings = Pair.cons(new Binding(new Var(define.getVar().getName()), rewrite(value)), bindings);
        }
      }
      else
      {
        operations.add(rewrite(exp));
      }
    }
    if (operations.isEmpty())
    {
      throw new StremeException("no operations in " + begin);
    }
    return nestedLets(bindings, new Begin(operations, Begin.Kind.EXPLICIT)); 
  }
  
  private Node nestedLets(Lst valueBindings, Node body)
  {
    Node result = body;
    while (!valueBindings.isNull())
    {
      Binding binding = (Binding) valueBindings.car();
      result = new Let(Let.Kind.LET, binding, result);
      valueBindings = (Lst) valueBindings.cdr();
    } 
    return result;
  }
  
  public static void main(String[] args)
  {
    Parser2 parser = new Parser2();
    AstDataCompiler compiler = new StremeDataCompiler();
    AnfConverter anfConverter = new AnfConverter(RenamingStrategy.NUMBER_RENAMING_STRATEGY);
    String source = "(begin (define trace? #f) (define (nqueens n) (define (_1-to n) (let loop ((i n) (l '())) (if (= i 0) l (loop (- i 1) (cons i l))))) (define (my-try x y z) (if (null? x) (if (null? y) (begin (if trace? (begin (write z) (newline))) 1) 0) (+ (if (ok? (car x) 1 z) (my-try (append (cdr x) y) '() (cons (car x) z)) 0) (my-try (cdr x) (cons (car x) y) z)))) (define (ok? row dist placed) (if (null? placed) #t (and (not (= (car placed) (+ row dist))) (not (= (car placed) (- row dist))) (ok? row (+ dist 1) (cdr placed))))) (my-try (_1-to n) '() '())) (equal? (nqueens 8) 14200))";
    Object parsed = parser.parse(source);
    MacroExpander expander = new MacroExpander();
    expander.setLetrecToLambda(false);
    expander.setLetToLambda(false);
    Object expanded = expander.rewrite(parsed);
    Node ast = compiler.compile(expanded);
    System.out.println("ast: " + Printer.print(ast));
    Node undefined = Undefiner.undefine(ast);
    System.out.println("und: " + Printer.print(undefined));
    Node anf = anfConverter.rewrite(undefined);
    System.out.println("anf: " + Printer.print(anf));
  }
  
}
