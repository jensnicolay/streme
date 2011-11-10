package streme.lang.ast.analysis.ipda;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Deque;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.logging.Logger;

import org.jgrapht.DirectedGraph;
import org.jgrapht.Graphs;
import org.jgrapht.alg.DijkstraShortestPath;
import org.jgrapht.graph.DefaultDirectedGraph;
import org.jgrapht.graph.DefaultEdge;
import org.jgrapht.graph.SimpleDirectedGraph;

import streme.lang.ast.analysis.GraphUtils;

public class DependencyGraphParallelizer
{
  public static final Logger LOGGER = Logger.getLogger("dgp");
  
  public static class Vertex implements Iterable<Object>
  {
    private static int counter;
    private int tag = counter++;
    private List<Object> vs;
    private Map<String, Object> properties;

    public Vertex(Object... vs)
    {
      this(Arrays.asList(vs));
    }

    public Vertex(List<Object> vs)
    {
      super();
      this.vs = new ArrayList<Object>(vs);
      properties = new HashMap<String, Object>();
    }
    
    public int getTag()
    {
      return tag;
    }
    
    public void setProperty(String name, Object value)
    {
      properties.put(name, value);
    }
    
    public Object getProperty(String name)
    {
      return properties.get(name);
    }

    public Iterator<Object> iterator()
    {
      return vs.iterator();
    }

    public List<Object> getValues()
    {
      return new ArrayList<Object>(vs);
    }

    public void add(Object v)
    {
      vs.add(v);
    }

    public void addAll(List<Object> vs)
    {
      this.vs.addAll(vs);
    }

    public String toString()
    {
      return vs.toString();
    }
  }
  
  public static class Edge extends DefaultEdge
  {
    private static int counter;
    private int tag = counter++;
    private Map<String, Object> properties;
    
    public Edge()
    {
      super();
      properties = new HashMap<String, Object>();
    }
    
    public int getTag()
    {
      return tag;
    }
    
    public void setProperty(String name, Object value)
    {
      properties.put(name, value);
    }
    
    public Object getProperty(String name)
    {
      return properties.get(name);
    }
    
    public Map<String, Object> getProperties()
    {
      return properties;
    }
    
    public void putAllProperties(Map<String, Object> properties)
    {
      this.properties.putAll(properties);
    }

  }
  
//  public static class Edge extends DefaultEdge
//  {
//    
//  }
  
  public DirectedGraph<Vertex, Edge> verticize(DirectedGraph<?, Edge> g)
  {
    Map<Object, Vertex> vs = new HashMap<Object, Vertex>();
    DirectedGraph<Vertex, Edge> result = new DefaultDirectedGraph<Vertex, Edge>(Edge.class);
    for (Object o : g.vertexSet())
    {
      Vertex v = new Vertex(o);
      result.addVertex(v);
      vs.put(o, v);
    }
    for (Edge e : g.edgeSet())
    {
      Edge newEdge = result.addEdge(vs.get(g.getEdgeSource(e)), vs.get(g.getEdgeTarget(e)));
      Map<String, Object> properties = e.getProperties();
      newEdge.putAllProperties(properties);
    }
    return result;
  }

  private void fold(DirectedGraph<Vertex, Edge> g, Vertex source, Vertex target)
  {
    List<Vertex> targetIn = Graphs.predecessorListOf(g, target);
    List<Vertex> targetOut = Graphs.successorListOf(g, target);
    source.addAll(target.getValues());
    g.removeVertex(target);
    for (Vertex ti : targetIn)
    {
      if (!ti.equals(source))
      {
        g.addEdge(ti, source);
      }
    }
    for (Vertex to : targetOut)
    {
      g.addEdge(source, to);
    }
  }

  public void spiderCompact(DirectedGraph<Vertex, Edge> g)
  {
    boolean modified = true;
    while (modified)
    {
      modified = false;
      for (Vertex source : new HashSet<Vertex>(g.vertexSet()))
      {
        Set<Edge> out = g.outgoingEdgesOf(source);
        if (out.size() == 1)
        {
          Vertex target = g.getEdgeTarget(out.iterator().next());
          if (/*g.inDegreeOf(source) == 0 ||*/ g.inDegreeOf(target) == 1)
          {
            fold(g, source, target);
            LOGGER.fine("folded " + target + " into " + source);
            modified = true;
            break;
          }
        }
      }
    }
  }
  
  public void prune(DirectedGraph<Vertex, Edge> g)
  {
    Deque<Vertex> todo = new ArrayDeque<Vertex>(g.vertexSet());
    Set<Object> seen = new HashSet<Object>();
    while (!todo.isEmpty())
    {
      Vertex v = todo.pop();
      if (seen.contains(v))
      {
        continue;
      }
      seen.add(v);
      List<Vertex> targets = Graphs.successorListOf(g, v);
      for (int i = 0; i < targets.size(); i++)
      {
        for (int j = i; j < targets.size(); j++)
        {
          Vertex vi = targets.get(i);
          Vertex vj = targets.get(j);
          List<Edge> path = DijkstraShortestPath.findPathBetween(g, vi, vj);
          // System.out.println("checking " + vi + "->" + vj + " for " + v);
          if (path != null && !path.isEmpty())
          {
            Edge edge = g.getEdge(v, vj);
            g.removeEdge(edge);
            LOGGER.info("\tremoved " + edge + " because of " + path + " for " + v);
          }
        }
      }
    }
  }
  
  public List<List<Vertex>> toplogicalLevelSort(DirectedGraph<Vertex, Edge> g)
  {
    Map<Integer, List<Vertex>> levels = new HashMap<Integer, List<Vertex>>();
    List<Vertex> sinks = GraphUtils.getSinks(g);
    if (sinks.size() != 1)
    {
      throw new IllegalArgumentException("expected 1 sink, got " + sinks);
    }
    Vertex sink = sinks.get(0);
    for (Vertex v : g.vertexSet())
    {
      List<List<Vertex>> paths = GraphUtils.findAllPaths(g, v, sink);
      int max = 0;
      for (List<Vertex> p : paths)
      {
        max = Math.max(p.size(), max);
      }
      LOGGER.finer(v + " level " + max);
      List<Vertex> level = levels.get(max);
      if (level == null)
      {
        level = new ArrayList<Vertex>();
        levels.put(max, level);
      }
      level.add(v);
    }
    List<List<Vertex>> result = new ArrayList<List<Vertex>>();
    for (int level : new TreeSet<Integer>(levels.keySet()))
    {
      result.add(levels.get(level));
    }
    Collections.reverse(result);
    return result;
  }


  public static void main(String[] args)
  {
    DirectedGraph<Vertex, Edge> g = connect2();
    DependencyGraphParallelizer dgp = new DependencyGraphParallelizer();
    dgp.spiderCompact(g);
    System.out.println(g);
  }

  private static Vertex addVertex(DirectedGraph<Vertex, Edge> g, Object o)
  {
    Vertex v = new Vertex(o);
    g.addVertex(v);
    return v;
  }

  private static DirectedGraph<Vertex, Edge> connect1()
  {
    DirectedGraph<Vertex, Edge> g = new SimpleDirectedGraph<Vertex, Edge>(Edge.class);
    Vertex v1 = addVertex(g, 1);
    Vertex v2 = addVertex(g, 2);
    Vertex v3 = addVertex(g, 3);
    Vertex v4 = addVertex(g, 4);
    Vertex v5 = addVertex(g, 5);
    Vertex v6 = addVertex(g, 6);
    Vertex v7 = addVertex(g, 7);
    g.addEdge(v1, v4);
    g.addEdge(v2, v4);
    g.addEdge(v1, v6);
    g.addEdge(v3, v6);
    g.addEdge(v4, v5);
    g.addEdge(v5, v7);
    g.addEdge(v6, v7);
    return g;
  }

  private static DirectedGraph<Vertex, Edge> connect2()
  {
    DirectedGraph<Vertex, Edge> g = new SimpleDirectedGraph<Vertex, Edge>(Edge.class);
    Vertex v1 = addVertex(g, 1);
    Vertex v2 = addVertex(g, 2);
    Vertex v3 = addVertex(g, 3);
    Vertex v4 = addVertex(g, 4);
    Vertex v5 = addVertex(g, 5);
    Vertex v6 = addVertex(g, 6);
    Vertex v7 = addVertex(g, 7);
    g.addEdge(v1, v2);
    g.addEdge(v1, v3);
    g.addEdge(v2, v5);
    g.addEdge(v4, v5);
    g.addEdge(v3, v6);
    g.addEdge(v4, v6);
    g.addEdge(v5, v7);
    g.addEdge(v6, v7);
    return g;
  }
}
