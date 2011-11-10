package streme.lang.ast.impl;

import streme.lang.ast.Application;
import streme.lang.ast.AstDataCompiler;
import streme.lang.ast.Binding;
import streme.lang.ast.Future;
import streme.lang.ast.Let;
import streme.lang.ast.Node;
import streme.lang.ast.Ref;
import streme.lang.ast.Var;
import streme.lang.ast.analysis.RenamingStrategy;
import streme.lang.data.Parser2;
import streme.lang.data.Sym;

public class ParallelConstructsRewriter extends RecursiveDescentAstRewriter
{
  private static final RenamingStrategy renamer = RenamingStrategy.NUMBER_RENAMING_STRATEGY;
  private static final Ref TOUCH = new Ref(new Sym("touch"));
  
  public Node rewriteLet(Let let)
  {
    if (let.getKind() == Let.Kind.LETPAR)
    {

      Binding[] bindings = let.getBindings();
      int numBindings = bindings.length;
      Binding[] futureBindings = new Binding[numBindings];
      Binding[] touchBindings = new Binding[numBindings - 1];
      int i;
      for (i = 0; i < numBindings - 1; i++)
      {
        Sym name = bindings[i].getVar().getName();
        Sym rname = renamer.rename(name);
        futureBindings[i] = new Binding(new Var(rname), new Future(bindings[i].getValue()));
        touchBindings[i] = new Binding(new Var(name), new Application(TOUCH, new Ref(rname)));
      }
      futureBindings[i] = bindings[i];
      return rewrite(new Let(Let.Kind.LET, futureBindings, new Let(Let.Kind.LET, touchBindings, let.getBody())));
    }
    return super.rewriteLet(let);
  }

  public static void main(String[] args)
  {
    ParallelConstructsRewriter pcr = new ParallelConstructsRewriter();
    Parser2 parser = new Parser2();
    AstDataCompiler compiler = new StremeDataCompiler();
    String source = "(let|| ((a 2) (b (+ c 5))) (* a b))";
    System.out.println(pcr.rewrite(compiler.compile(parser.parse(source))));
  }
}
