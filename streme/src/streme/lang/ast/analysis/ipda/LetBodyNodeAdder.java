package streme.lang.ast.analysis.ipda;

import java.util.ArrayList;
import java.util.List;

import streme.lang.ast.AstDataCompiler;
import streme.lang.ast.Let;
import streme.lang.ast.Node;
import streme.lang.ast.Node.Type;
import streme.lang.ast.Ref;
import streme.lang.ast.Var;
import streme.lang.ast.analysis.RenamingStrategy;
import streme.lang.ast.impl.RecursiveDescentAstRewriter;
import streme.lang.ast.impl.StremeDataCompiler;
import streme.lang.data.Parser2;
import streme.lang.data.Sym;

public class LetBodyNodeAdder extends RecursiveDescentAstRewriter
{
  
  private RenamingStrategy rs;

  public LetBodyNodeAdder(RenamingStrategy rs)
  {
    super();
    this.rs = rs;
  }

  protected Node rewriteLet(final Let original)
  {
    if (original.isSimpleLet())
    {
      List<Let> lets = new ArrayList<Let>();
      lets.add(original);
      Node body = original.getBody();
      while (body.type() == Type.LET)
      {
        Let blet = (Let) body;
        if (blet.isSimpleLet())
        {
          lets.add(blet);
          body = blet.getBody();
        }
        else
        {
          break;
        }
      }
      if (lets.size() > 2 && body.type() != Type.LITERAL && body.type() != Type.REF && body.type() != Type.LAMBDA)
      {
        Sym bodyName = rs.rename(new Sym("body"));
        lets.add(new Let(Let.Kind.LET, new Var(bodyName), body, new Ref(bodyName)));
        return toNestedLets(lets);
      }
    }
    return new Let(original.getKind(), rewriteBindings(original.getBindings()), rewrite(original.getBody()));
  }
  
  private Node toNestedLets(List<Let> lets)
  {
    Let let = lets.get(lets.size() - 1);
    for (int i = lets.size() - 2; i > -1; i--)
    {
      Let l = lets.get(i);
      let = new Let(Let.Kind.LET, l.getBindings()[0], let);
    }
    return let;
  }

  public static void main(String[] args)
  {
    Parser2 parser = new Parser2();
    Object data = parser.parse("(let ((x 1)) (let ((y (f 2))) (let ((z 3)) 'j)))");
    AstDataCompiler compiler = new StremeDataCompiler();
    Node ast = compiler.compile(data);
    System.out.println(ast);
    LetBodyNodeAdder lbar = new LetBodyNodeAdder(RenamingStrategy.NUMBER_RENAMING_STRATEGY);
    Node rast = lbar.rewrite(ast);
    System.out.println(rast);
  }
}
