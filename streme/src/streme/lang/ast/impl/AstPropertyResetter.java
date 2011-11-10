package streme.lang.ast.impl;

import streme.lang.ast.AstVisitor;
import streme.lang.ast.Node;

public class AstPropertyResetter extends AstVisitor
{
  
  public static void resetProperties(Node ast, String... properties)
  {
    ast.accept(new AstPropertyResetter(properties));
  }
  
  private String[] properties;
  
  public AstPropertyResetter(String... properties)
  {
    super();
    this.properties = properties;
  }
  
  public boolean visitNode(Node node)
  {
    for (String p : properties)
    {
      node.resetProperty(p);
    }
    return true;
  }
}
