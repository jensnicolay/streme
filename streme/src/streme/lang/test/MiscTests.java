package streme.lang.test;

import junit.framework.TestCase;
import streme.lang.data.Lst;

public class MiscTests extends TestCase
{
  
  public void testTake()
  {
    Lst l = Lst.valueOf(1, 2, 3);
    assertEquals("()", l.take(0).toString());
    assertEquals("(1)", l.take(1).toString());
    assertEquals("(1 2)", l.take(2).toString());
    assertEquals("(1 2 3)", l.take(3).toString());
  }
}
