package streme.lang.analysis;

import java.util.HashMap;
import java.util.Map;

import streme.lang.ast.AstAnalyzer;
import streme.lang.ast.AstDataCompiler;
import streme.lang.ast.AstVisitor;
import streme.lang.ast.Node;
import streme.lang.ast.impl.NodeFinders;
import streme.lang.ast.impl.StremeDataCompiler;
import streme.lang.data.Parser2;

/**
 * Links every node with its parent.
 */
public class ParentAnalyzer implements AstAnalyzer<ParentAnalysis>
{
  public static Node getParent(Node ast, Node node)
  {
    return new ParentAnalyzer().analyze(ast).getParent(node);
  }
  
  public static Node getRoot(Node node, ParentAnalysis parentAnalysis)
  {
    Node parent;
    while ((parent = parentAnalysis.getParent(node)) != null)
    {
      node = parent;
    }
    return node;
  }

  public ParentAnalyzer()
  {
    super();
  }

  public ParentAnalysis analyze(Node node)
  {
    final Map<Node, Node> parents = new HashMap<Node, Node>();
    node.accept(new AstVisitor()
    {
      public boolean visitNode(Node node)
      {
        for (Object child : node.children())
        {
          Node c = (Node) child;
          parents.put(c, node);
          c.accept(this);
        }
        return false;
      }
    });
    return new ParentAnalysis()
    {
      public Node getParent(Node node)
      {
        return parents.get(node);
      }
    };
  }

  public static void main(String[] args)
  {
    Parser2 parser = new Parser2();
    AstDataCompiler compiler = new StremeDataCompiler();
    Node ast = compiler.compile(parser.parse("(let ((x 1) (y 2)) (+ x y))"));
    ParentAnalysis parentAnalysis = new ParentAnalyzer().analyze(ast);
    System.out.println(parentAnalysis.getParent(ast));
    System.out.println(parentAnalysis.getParent(NodeFinders.findUnifyingNode(ast, parser.parse("(x 1)"))));
    System.out.println(parentAnalysis.getParent(NodeFinders.findUnifyingNode(ast, parser.parse("2"))));
    System.out.println(parentAnalysis.getParent(NodeFinders.findUnifyingNode(ast, parser.parse("(+ x y)"))));
  }
}
