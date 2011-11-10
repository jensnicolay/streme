package streme.lang.eval.nd.test;

import java.util.List;

import junit.framework.TestCase;
import streme.lang.ast.Node;
import streme.lang.ast.impl.SpPrinter;
import streme.lang.eval.nd.Asts;
import streme.lang.eval.nd.NdAnalysisStreme;

public class NdAnalysisTransformationTests extends TestCase
{
  
  public void testIdTransformation()
  {
    idTransform("(let ((a 1) (b 2)) (+ a b))");
    idTransform("(  let   ((a 1 ) (  b 2) ) (  + a    b)  )");
    idTransform("  (if  (  +   ( * 2 3 5)) (begin 1\t\t\t2 3 \n3 44 \"5\"  ) (\tlambda (  xx yyy    )  (\n- (  +  (map    xx    yy   ) )\t)\n))");
  }

  public void idTransform(String source)
  {
    Node ast = Asts.createAst(source);
    NdAnalysisStreme analysisStreme = new NdAnalysisStreme(ast);
    String meta = "(rewrite *ast* (lambda (node) #f))";
    List<Node> nodes = analysisStreme.queryNodes(meta);
    assertEquals(1, nodes.size());
    Node node = nodes.get(0);
    assertTrue(node.nodeEquals(ast));
    assertEquals(source, SpPrinter.print(node));
  }

}
