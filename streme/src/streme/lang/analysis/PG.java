package streme.lang.analysis;

import java.util.Arrays;
import java.util.Iterator;

public class PG implements Iterator<int[]>, Iterable<int[]>
{
  private int[] v;
  private int[] m;
  private int p;

  public PG(int[] m)
  {
    super();
    this.m = m;
    this.p = m.length;
    v = new int[p];
  }

  public int[] next()
  {
    if (v == null)
    {
      return null;
    }
    int[] result = v.clone();
    v[0]++;
    int i = 0;
    while (v[i] >= m[i++])
    {
      if (i == p)
      {
        v = null;
        break;
      }
      v[i - 1] = 0;
      v[i]++;
    }
    return result;
  }

  public boolean hasNext()
  {
    return v != null;
  }
  
  public Iterator<int[]> iterator()
  {
    return this;
  }

  public void remove()
  {
    throw new UnsupportedOperationException();
  }
  
  public static void main(String[] args)
  {
    for (int[] v : new PG(new int[] {3, 1, 2}))
    {
      System.out.println(Arrays.toString(v));
    }
  }
}