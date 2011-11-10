package streme.lang.ast.impl;

import java.util.EnumMap;
import java.util.HashMap;
import java.util.Map;

import streme.lang.ast.AstVisitor;
import streme.lang.ast.Node;
import streme.lang.data.DataUnifier;
import streme.lang.data.Parser2;

public class NodeCounter extends AstVisitor
{
  private int total;
  private Map<Node.Type, Integer> typeCounters;
  private Map<Object, Integer> patternCounters;
  private DataUnifier unifier;
  
  public NodeCounter()
  {
    super();
    typeCounters = new EnumMap<Node.Type, Integer>(Node.Type.class);
    patternCounters = new HashMap<Object, Integer>();
    unifier = new DataUnifier();
  }
  
  public void countPattern(Object pattern)
  {
    patternCounters.put(pattern, 0);
  }
  
  public boolean visitNode(Node node)
  {
    total++;
    Node.Type type = node.type();
    Integer c = typeCounters.get(type);
    if (c == null)
    {
      typeCounters.put(type, 1);
    }
    else
    {
      typeCounters.put(type, c + 1);
    }
    for (Map.Entry<Object, Integer> kv : patternCounters.entrySet())
    {
      Object nodeData = node.toData();
      Object key = kv.getKey();
      if (nodeData == null)
      {
        if (key == null)
        {
          kv.setValue(kv.getValue() + 1);
        }
      }
      else if (key != null && unifier.matches(nodeData, key))
      {
        kv.setValue(kv.getValue() + 1);
      }
    }
    return true;
  }
  
  public int getTypeCount(Node.Type type)
  {
    Integer c = typeCounters.get(type);
    return c == null ? 0 : c;
  }
  
  public int getPatternCount(Object pattern)
  {
    Integer c = patternCounters.get(pattern);
    return c == null ? 0 : c;
  }
  
  public int getTotalCount()
  {
    return total;
  }
  
  public static void main(String[] args)
  {
    Parser2 parser = new Parser2();
    StremeDataCompiler compiler = new StremeDataCompiler();
    String source = "(define f (lambda (x) (future f) (future (future h)) (touch (touch y))))";
    Node ast = compiler.compile(parser.parse(source));
    NodeCounter nc = new NodeCounter();
    Object futureP = parser.parse("(future ?)");
    nc.countPattern(futureP);
    Object touchP = parser.parse("(touch ?)");
    nc.countPattern(touchP);
    ast.accept(nc);
    System.out.println(nc.getTotalCount());
    System.out.println("apps type " + nc.getTypeCount(Node.Type.APPLICATION));
    System.out.println("futures type " + nc.getTypeCount(Node.Type.FUTURE));
    System.out.println("futures P " + nc.getPatternCount(futureP));
    System.out.println("touches P " + nc.getPatternCount(touchP));
  }
}
