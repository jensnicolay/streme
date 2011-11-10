package streme.lang.eval.nd;

import streme.lang.ast.Node;
import streme.lang.ast.impl.StremeSpDataCompiler2;
import streme.lang.data.SpParser2;

public class Asts
{
  
  public static Node createAst(String source)
  {
    SpParser2 parser = new SpParser2(source);
    Object data = parser.next();
    StremeSpDataCompiler2 compiler = new StremeSpDataCompiler2(parser.getSps());
    Node ast = compiler.compile(data);
    return ast;
  }

  private Asts()
  {
    super();
  }
}
