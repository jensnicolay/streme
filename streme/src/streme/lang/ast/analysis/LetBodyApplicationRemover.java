package streme.lang.ast.analysis;

import streme.lang.ast.AstDataCompiler;
import streme.lang.ast.Let;
import streme.lang.ast.Node;
import streme.lang.ast.Node.Type;
import streme.lang.ast.Ref;
import streme.lang.ast.Var;
import streme.lang.ast.impl.RecursiveDescentAstRewriter;
import streme.lang.ast.impl.StremeDataCompiler;
import streme.lang.data.Parser2;
import streme.lang.data.Sym;

public class LetBodyApplicationRemover extends RecursiveDescentAstRewriter
{
  
  private RenamingStrategy rs;

  public LetBodyApplicationRemover(RenamingStrategy rs)
  {
    super();
    this.rs = rs;
  }

  protected Node rewriteLet(Let let)
  {
    Type type = let.getBody().type();
    if (type == Type.APPLICATION)
    {
      Sym name = rs.rename(new Sym("lba"));
      Let rewritten = new Let(let.getKind(), rewriteBindings(let.getBindings()), new Let(Let.Kind.LET, new Var(name), let.getBody(), new Ref(name)));
      return rewritten;
    }
    return super.rewriteLet(let);
  }
  
  public static void main(String[] args)
  {
    Parser2 parser = new Parser2();
    Object data = parser.parse("(let ((x 1)) (let ((y (f 2))) (let ((z 3)) (g 4 5 6))))");
    AstDataCompiler compiler = new StremeDataCompiler();
    Node ast = compiler.compile(data);
    System.out.println(ast);
    LetBodyApplicationRemover lbar = new LetBodyApplicationRemover(RenamingStrategy.NUMBER_RENAMING_STRATEGY);
    Node rast = lbar.rewrite(ast);
    System.out.println(rast);
  }
}
