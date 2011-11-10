package streme.lang.ast.impl;

import java.util.ArrayList;
import java.util.List;

import streme.lang.StremeException;
import streme.lang.ast.AstDataCompiler;
import streme.lang.ast.AstVisitor;
import streme.lang.ast.Node;
import streme.lang.data.Pair;
import streme.lang.data.Parser2;

public class NodeMatcher<T> extends AstVisitor
{
  
  public interface Matcher<T>
  {
    /**
     * @return null when no match, information about the match otherwise
     */
    T matches(Node node);
  }
  
  public static <T> List<Pair<Node, T>> findMatches(Node ast, Matcher<T> strategy)
  {
    NodeMatcher<T> nodeFinder = new NodeMatcher<T>(strategy);
    ast.accept(nodeFinder);
    return nodeFinder.getMatches();
  }

  public static <T> Pair<Node, T> findMatch(Node ast, Matcher<T> matcher)
  {
    List<Pair<Node, T>> nodes = findMatches(ast, matcher);
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

  private Matcher<T> matcher;
  private List<Pair<Node, T>> result;

  public NodeMatcher(Matcher<T> matcher)
  {
    super();
    this.matcher = matcher;
    result = new ArrayList<Pair<Node, T>>();
  }

  public boolean visitNode(Node node)
  {
    T o = matcher.matches(node);
    if (o != null)
    {
      result.add(Pair.cons(node, o));
    }
    return true;
  }

  public List<Pair<Node, T>> getMatches()
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