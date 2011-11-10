package streme.lang.ast.impl;

import java.util.List;

import streme.lang.ast.AstDataCompiler;
import streme.lang.ast.Node;
import streme.lang.ast.impl.NodeFinder.Matcher;
import streme.lang.data.DataUnifier;
import streme.lang.data.Parser2;

public class NodeFinders
{
  
  public static class UnifyingMatcher implements Matcher
  {
    
    private Object[] patterns;
    private DataUnifier unifier;
    
    public UnifyingMatcher(Object... patterns)
    {
      super();
      this.patterns = patterns;
      unifier = new DataUnifier();
    }
    
    public boolean matches(Node node)
    {
      Object data = node.toData();
      for (Object pattern : patterns)
      {
        if (unifier.matches(pattern, data))
        {
          return true;
        }
      }
      return false;
    }
  }
  
  public static class EqualsMatcher implements Matcher
  {
    private Node node;
    
    public EqualsMatcher(Node node)
    {
      super();
      this.node = node;
    }
    
    public boolean matches(Node node)
    {
      return this.node.nodeEquals(node);
    }
  }
    
  public static List<Node> findUnifyingNodes(Node ast, Object... patterns)
  {
    return NodeFinder.findNodes(ast, new UnifyingMatcher(patterns));
  }

  public static Node findUnifyingNode(Node ast, Object... patterns)
  {
    return NodeFinder.findNode(ast, new UnifyingMatcher(patterns));
  }
 
  public static List<Node> findEqualNodes(Node ast, Node node)
  {
    return NodeFinder.findNodes(ast, new EqualsMatcher(node));
  }

  public static Node findEqualNode(Node ast, Node node)
  {
    return NodeFinder.findNode(ast, new EqualsMatcher(node));
  }
 
  public static void main(String[] args)
  {
    String source = "(let* ((z 0) (writez (lambda () (set! z 123))) (readz (lambda () z)) (a (readz)) (b (writez))) (h a b))";
    Parser2 parser = new Parser2();
    AstDataCompiler compiler = new StremeDataCompiler();
    Node ast = compiler.compile(parser.parse(source));
    System.out.println(NodeFinders.findUnifyingNodes(ast, parser.parse("(readz)")));
  }
}