package streme.lang.ast.impl;

import streme.lang.ast.AstRewriter;
import streme.lang.ast.Node;
import streme.lang.data.Lst;
import streme.lang.data.Null;
import streme.lang.data.Pair;

public class NodeReplacer implements AstRewriter
{
  interface Matcher
  {
    boolean matches(Node n1, Node n2);
  }

  public static final Matcher EQUALS_MATCHER = new Matcher()
  {
    public boolean matches(Node n1, Node n2)
    {
      return n1.nodeEquals(n2);
    }
  };
  
  public static Node replaceEqual(Node node, Node from, Node to)
  {
    NodeReplacer nr = new NodeReplacer(EQUALS_MATCHER);
    nr.addRewrite(from, to);
    return nr.rewrite(node);
  }
  
  private Lst rewrites;
  private Matcher matcher;

  public NodeReplacer(Matcher matcher)
  {
    super();
    this.matcher = matcher;
    rewrites = new Null();
  }

  public void addRewrite(Node from, Node to)
  {
    rewrites = Pair.cons(Pair.cons(from, to), rewrites);
  }

  public Node rewrite(Node ast)
  {
    AstRewriter rewriter = new RecursiveDescentAstRewriter()
    {
      public Node rewrite(Node node)
      {
        for (Object rObj : rewrites)
        {
          Pair<Node, Node> r = (Pair<Node, Node>) rObj;
          if (matcher.matches(r.car(), node))
          {
            return r.cdr();
          }
        }
        return super.rewrite(node);
      }
    };
    return rewriter.rewrite(ast);
  }
  
  public static void main(String[] args)
  {
    
  }
}
