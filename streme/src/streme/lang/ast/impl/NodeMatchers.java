package streme.lang.ast.impl;

import java.util.List;
import java.util.Map;

import streme.lang.ast.AstDataCompiler;
import streme.lang.ast.Node;
import streme.lang.ast.impl.NodeMatcher.Matcher;
import streme.lang.data.DataUnifier;
import streme.lang.data.Pair;
import streme.lang.data.Parser2;
import streme.lang.data.Sym;

public class NodeMatchers
{
  
  public static class UnifyingMatcher implements Matcher<Map<Sym, Object>>
  {
    
    private Object[] patterns;
    private DataUnifier unifier;
    
    public UnifyingMatcher(Object... patterns)
    {
      super();
      this.patterns = patterns;
      unifier = new DataUnifier();
    }
    
    public Map<Sym, Object> matches(Node node)
    {
      Object data = node.toData();
      for (Object pattern : patterns)
      {
        Map<Sym, Object> u = unifier.unify(pattern, data);
        if (u != null)
        {
          return u;
        }
      }
      return null;
    }
  }
  
    
  public static List<Pair<Node, Map<Sym, Object>>> findUnifyingMatches(Node ast, Object... patterns)
  {
    return NodeMatcher.findMatches(ast, new UnifyingMatcher(patterns));
  }

  public static Pair<Node, Map<Sym, Object>> findUnifyingMatch(Node ast, Object... patterns)
  {
    return NodeMatcher.findMatch(ast, new UnifyingMatcher(patterns));
  }
  
  public static void main(String[] args)
  {
    String source = "(let* ((z 0) (writez (lambda () (set! z 123))) (readz (lambda () z)) (a (readz)) (b (writez))) (h a b))";
    Parser2 parser = new Parser2();
    AstDataCompiler compiler = new StremeDataCompiler();
    Node ast = compiler.compile(parser.parse(source));
    List<Pair<Node, Map<Sym, Object>>> matches = NodeMatchers.findUnifyingMatches(ast, parser.parse("(lambda () z)"));
    System.out.println(matches);
  }
}