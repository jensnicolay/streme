package streme.lang.analysis;

import streme.lang.ast.Node;

public interface ParentAnalysis
{
  Node getParent(Node node);
}
