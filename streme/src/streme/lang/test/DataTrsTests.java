package streme.lang.test;

import junit.framework.TestCase;
import streme.lang.data.Data;
import streme.lang.data.DataRule;
import streme.lang.data.DataTrs;
import streme.lang.data.Lst;
import streme.lang.data.Parser2;

public class DataTrsTests extends TestCase
{
  private Parser2 parser = new Parser2();

  private void rewrite(String in, String expected, String... rules)
  {
    Object n = parser.parse(in);
    DataTrs trs = new DataTrs();
    for (String rule : rules)
    {
      Lst r = (Lst) parser.parse(rule);
      DataRule rr = new DataRule(r.car(), r.cadr(), r.cddr());
      trs.addRule(rr);
    }
    Object rewrite = trs.rewrite(n);
    assertEquals(expected, Data.toString(rewrite));
  }

  public void testUnconditionalDataTrs()
  {
    rewrite("(or #t 123)", "#t", "(r1 (or #t ?x) . #t)");
    rewrite("((or #t 123))", "(#t)", "(r1 (or #t ?x) . #t)");
    rewrite("(or #t 123)", "#t", "(r1 (or #t ?x) . #t)", "(r2 (not (and ?x ?y)) . (or ?x ?y))");
    rewrite("(pred (and #t 123))", "#t", "(r1 (or #t ?x) . #t)", "(r2 (pred (and ?x ?y)) . (or ?x ?y))");
    rewrite("(or (or #t 123) 456)", "#t", "(r1 (or #t ?x) . #t)");
  }
  
}
