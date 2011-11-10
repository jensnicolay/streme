package streme.lang.ast.analysis;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.List;

import org.jgrapht.DirectedGraph;
import org.jgrapht.Graphs;
import org.jgrapht.graph.DefaultEdge;
import org.jgrapht.graph.SimpleDirectedGraph;

public class GraphUtils
{
  
  public static <V, E> String dumpGraph(DirectedGraph<V, E> g)
  {
    StringBuilder sb = new StringBuilder();
    for (V v : g.vertexSet())
    {
     sb.append("\n").append(v).append("\n");
     for (E e : g.outgoingEdgesOf(v))
     {
       sb.append("\t--> ").append(g.getEdgeTarget(e)).append("\n");
     }
    }
    return sb.toString();
  }
  
  public static <V, E> List<V> getSources(DirectedGraph<V, E> g)
  {
    List<V> vs = new ArrayList<V>();
    for (V v : g.vertexSet())
    {
      if (g.inDegreeOf(v) == 0)
      {
        vs.add(v);
      }
    }
    return vs;
  }
  
  public static <V, E> List<V> getSinks(DirectedGraph<V, E> g)
  {
    List<V> vs = new ArrayList<V>();
    for (V v : g.vertexSet())
    {
      if (g.outDegreeOf(v) == 0)
      {
        vs.add(v);
      }
    }
    return vs;
  }
  
  public static <V, E> List<List<V>> findAllPaths(DirectedGraph<V, E> g, V from, V to)
  {
    Deque<List<V>> q = new ArrayDeque<List<V>>();
    List<V> f = new ArrayList<V>();
    f.add(from);
    q.add(f);
    List<List<V>> r = new ArrayList<List<V>>();
    while (!q.isEmpty())
    {
      List<V> p = q.pop();
      V last = p.get(p.size() - 1);
      if (last.equals(to))
      {
        r.add(p);
      }
      else
      {
       List<V> succs = Graphs.successorListOf(g, last);
       for (V succ : succs)
       {
         List<V> pp = new ArrayList<V>(p);
         pp.add(succ);
         q.add(pp);
       }
      }
    }
    return r;
  }
  
  public static void main(String[] args)
  {
    DirectedGraph<Object, DefaultEdge> g = new SimpleDirectedGraph<Object, DefaultEdge>(DefaultEdge.class);
    g.addVertex("z");
    g.addVertex("s");
    g.addVertex("x");
    g.addVertex("y");
    g.addVertex("u");
    g.addVertex("u1");
    g.addVertex("u2");
    g.addVertex("r");
    g.addVertex("body");
    g.addEdge("z", "s");
    g.addEdge("z", "x");
    g.addEdge("z", "y");
    g.addEdge("s", "u");
    g.addEdge("x", "u");
    g.addEdge("x", "r");
    g.addEdge("y", "u");
    g.addEdge("y", "r");
    g.addEdge("u", "u1");
    g.addEdge("u", "u2");
    g.addEdge("r", "body");
    g.addEdge("u1", "body");
    g.addEdge("u2", "body");
    System.out.println(findAllPaths(g, "z", "body"));
  }
}
