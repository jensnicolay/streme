package streme.lang.eval;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import streme.lang.StremeException;

public class MapEnv
{
  private MapEnv parent;
  private Map<Object, Object> bindings;
  
  public MapEnv()
  {
    this(null);
  }
  
  public MapEnv(MapEnv parent)
  {
    super();
    this.parent = parent;
    bindings = new LinkedHashMap<Object, Object>();
  }
  
  public Object set(Object key, Object value)
  {
    MapEnv env = this;
    do
    {
      if (env.bindings.containsKey(key))
      {
        return env.bindings.put(key, value);
      }
      env = env.getParent();
    }
    while (env != null);
    throw new StremeException("unbound key " + key);
  }
  
  public Object add(Object key, Object value)
  {
    return bindings.put(key, value);
  }
  
  public Object get(Object key)
  {
    MapEnv env = this;
    do
    {
      Object value = env.bindings.get(key);
      if (value != null)
      {
        return value;
      }
      env = env.getParent();
    }
    while (env != null);
    return null;
  }
  
  public MapEnv getParent()
  {
    return parent;
  }
  
  public Map<Object, Object> getBindings()
  {
    return bindings;
  }
  
  public Map<Object, Object> flatten()
  {
    MapEnv mapEnv = this;
    List<MapEnv> mapEnvs = new ArrayList<MapEnv>();
    do
    {
      mapEnvs.add(mapEnv);
      mapEnv = mapEnv.getParent();
    }
    while (mapEnv != null);
    Map<Object, Object> map = new LinkedHashMap<Object, Object>();
    for (int i = mapEnvs.size() - 1; i >= 0; i--)
    {
      map.putAll(mapEnvs.get(i).bindings);
    }
    return map;
  }
}
