package streme.lang.ast.impl;

import java.util.Arrays;
import java.util.logging.Logger;

import streme.lang.ast.Begin;
import streme.lang.ast.Binding;
import streme.lang.ast.Let;
import streme.lang.ast.Node;
import streme.lang.ast.Ref;
import streme.lang.data.Lst;
import streme.lang.data.Sym;

public class SyntaxSimplifier extends RecursiveDescentAstRewriter
{
  public static final Logger LOGGER = Logger.getLogger("simplifier");
  
  public static final Node simplify(Node node)
  {
    return new SyntaxSimplifier().rewrite(node);
  }
  
  protected Node rewriteBegin(Begin begin)
  {
    Lst children = begin.children();
    if (children.length() == 1)
    {
      LOGGER.fine("(begin x) => x");
      return rewrite((Node) children.car());
    }
    return new Begin(rewrite(begin.getExps(), Node.class), begin.getKind());
  }
  
  protected Node rewriteLet(Let let)
  {
    if (let.getKind() != Let.Kind.LETREC)
    {
      int lastIndex = let.getBindings().length - 1;
      if (lastIndex > -1)
      {
        Sym lastName = let.getName(lastIndex).getName();
        Node body = let.getBody();
        if (new Ref(lastName).nodeEquals(body))
        {
          if (lastIndex == 0)
          {
            LOGGER.fine("(let/let*/let|| ((x v)) x) => v");
            return let.getValue(lastIndex);
          }
          Binding[] newBindings = Arrays.copyOf(let.getBindings(), lastIndex);
          LOGGER.fine("(let/let*/let|| ( ... (x v)) x) => (let/let*/letrec (...) v)");
          return new Let(let.getKind(), rewriteBindings(newBindings), rewrite(let.getValue(lastIndex)));
        }
      }
    }  
    return new Let(let.getKind(), rewriteBindings(let.getBindings()), rewrite(let.getBody()));
  }
}
