package streme.lang.test;


public class Testo
{
  public static void main(String[] args)
  {
    char[] c = new char[2000000];
    for (int i = 0; i < c.length; i++)
    {
      c[i] = (char) (32 + (int) (Math.random() * 64));
    }
    String s = new String(c);
    //System.out.println(s);
    long start = System.currentTimeMillis();
    int h = s.hashCode();
    long end = System.currentTimeMillis();
    System.out.println((end - start));
  }
}
