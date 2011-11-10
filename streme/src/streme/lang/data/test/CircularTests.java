package streme.lang.data.test;

import java.math.BigInteger;

import junit.framework.TestCase;

import org.junit.Test;

import streme.lang.data.Null;
import streme.lang.data.Pair;

public class CircularTests extends TestCase
{

  @Test
  public void test1()
  {
    Pair p = Pair.cons(new BigInteger("1"), new Null());
    p.setCdr(p);
    assertEquals("(1 . °0°)", p.toString());
  }

  @Test
  public void test2()
  {
    Pair p = Pair.cons(new BigInteger("1"), Pair.cons(new BigInteger("2"), new Null()));
    ((Pair) p.cdr()).setCdr(p);
    assertEquals("(1 2 . °0°)", p.toString());
  }

  @Test
  public void test3()
  {
    Pair p = Pair.cons(new BigInteger("1"), Pair.cons(new BigInteger("2"), new Null()));
    p.setCar(p);
    assertEquals("(°0° 2)", p.toString());
  }
}
