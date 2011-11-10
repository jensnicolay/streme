package streme.lang.ast.impl;

import java.util.ArrayList;
import java.util.List;

import streme.lang.StremeException;
import streme.lang.ast.AstDataCompiler;
import streme.lang.ast.AstVisitor;
import streme.lang.ast.Node;
import streme.lang.data.Parser2;

public class NodeFinder extends AstVisitor
{
  
  public interface Matcher
  {
    boolean matches(Node node);
  }
  
  public static List<Node> findNodes(Node ast, Matcher strategy)
  {
    NodeFinder nodeFinder = new NodeFinder(strategy);
    ast.accept(nodeFinder);
    return nodeFinder.getNodes();
  }

  public static Node findNode(Node ast, Matcher matcher)
  {
    List<Node> nodes = findNodes(ast, matcher);
    switch (nodes.size())
    {
      case 0:
        return null;
      case 1:
        return nodes.get(0);
      default:
        throw new StremeException("multiple nodes for " + matcher + ": " + nodes);
    }
  }

  private Matcher matcher;
  private List<Node> result;

  public NodeFinder(Matcher matcher)
  {
    super();
    this.matcher = matcher;
    result = new ArrayList<Node>();
  }

  public boolean visitNode(Node node)
  {
    if (matcher.matches(node))
    {
      result.add(node);
    }
    return true;
  }

  public List<Node> getNodes()
  {
    return result;
  }
  
  public static void main(String[] args)
  {
    String source = "(let* ((z 0) (writez (lambda () (set! z 123))) (readz (lambda () z)) (a (readz)) (b (writez))) (h a b))";
    Parser2 parser = new Parser2();
    AstDataCompiler compiler = new StremeDataCompiler();
    Node ast = compiler.compile(parser.parse(source));
  }
}