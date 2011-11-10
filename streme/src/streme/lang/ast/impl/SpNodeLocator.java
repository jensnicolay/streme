package streme.lang.ast.impl;

import streme.lang.ast.AstVisitor;
import streme.lang.ast.Node;
import streme.lang.data.SpData;
import streme.lang.data.SpParser2;

public class SpNodeLocator extends AstVisitor
{
  
  public static Node locateCoveringNode(Node ast, int pos, int length)
  {
    SpNodeLocator locator = new SpNodeLocator(pos, length);
    ast.accept(locator);
    return locator.getCovering();
  }
  
  public static Node locateCoveredNode(Node ast, int pos, int length)
  {
    SpNodeLocator locator = new SpNodeLocator(pos, length);
    ast.accept(locator);
    return locator.getCovered();
  }
  
  private int pos;
  private int endPos;
  private Node bestCovering;
  private int bestInclusiveDistance;
  private Node bestCovered;
  private int bestExclusiveDistance;
  
  public SpNodeLocator(int pos, int length)
  {
    super();
    this.pos = pos;
    endPos = pos + length;
    bestInclusiveDistance =  Integer.MAX_VALUE;
    bestExclusiveDistance =  Integer.MAX_VALUE;
  }
  
  public boolean visitNode(Node node)
  {
    SpData sp = (SpData) node.getProperty("sp");
    int nodePos = sp.getPos();
    int nodeEndPos = sp.getEndPos();
    boolean inclusive = nodePos <= pos && endPos <= nodeEndPos;
    if (inclusive)
    {
      int inclusiveDistance = (pos - nodePos) + (nodeEndPos - endPos);
      if (inclusiveDistance < bestInclusiveDistance)
      {
        bestInclusiveDistance = inclusiveDistance;
        bestCovering = node;
      }
    }
    boolean exclusive = pos <= nodePos && nodeEndPos <= endPos;
    if (exclusive)
    {
      int exclusiveDistance = (nodePos - pos) + (endPos - nodeEndPos);
      if (exclusiveDistance < bestExclusiveDistance)
      {
        bestExclusiveDistance = exclusiveDistance;
        bestCovered = node;
      }
    }
    return inclusive || exclusive;
  }
  
  public Node getCovering()
  {
    return bestCovering;
  }
  
  public Node getCovered()
  {
    return bestCovered;
  }
  
  public static void main(String[] args)
  {
    String source = "(define square (lambda (x) (set! x 123) (* x x)))";
    SpParser2 parser = new SpParser2(source);
    StremeSpDataCompiler2 compiler = new StremeSpDataCompiler2(parser.getSps());
    System.out.println(source);
    Node ast = compiler.compile(parser.next());
    //int i = 18;
    for (int i = 0; i < source.length() - 7; i++)
    {
      SpNodeLocator locator = new SpNodeLocator(i, 7);
      ast.accept(locator);
      System.out.println(i + "|"  + source.substring(i, i + 8) + "| " + locator.getCovering() + " $ " + locator.getCovered());
    }
  }
}
