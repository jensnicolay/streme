package streme.lang.test;

import java.util.Map;

import junit.framework.TestCase;
import streme.lang.data.Lst;
import streme.lang.data.Pair;
import streme.lang.data.SpData;
import streme.lang.data.SpParser2;

public class SpParserTests extends TestCase
{
  
  public void testos()
  {
    doCorrectMatchingTest("#t");
    doCorrectMatchingTest("#f");
    doCorrectMatchingTest("'hey");
    doCorrectMatchingTest("1234");
    doCorrectMatchingTest("567.89");
    doCorrectMatchingTest("\"yo yo yo\"");
    doCorrectMatchingTest("identifier");
    doCorrectMatchingTest("-");
    doCorrectMatchingTest("-123");
    doCorrectMatchingTest("(a b c)");
    doCorrectMatchingTest("(a b . c)");
    doCorrectMatchingTest("(a a 1 1 'hey 'hey \"ho\" \"ho\" 1.2 1.2 () () #t #t #f #f)");
    doCorrectMatchingTest("()");
    doCorrectMatchingTest("'()");
    doCorrectMatchingTest("#()");
    doCorrectMatchingTest("('dit is 334 \"test\")");
    doCorrectMatchingTest("#('dit is 334 \"test\")");
    doCorrectMatchingTest("(lambda () 'hey)");
    doCorrectMatchingTest("(let ((a 3) (b 4.5)) (+ a b))");
    doCorrectMatchingTest("(set!    \nx\t1)");
  }
  
  public void testLambda()
  {
    String source = "(lambda (a b c) 42.76)";
    Pair<Object, Map<Object, SpData>> p = doCorrectMatchingTest(source);
    Map<Object, SpData> sps = p.cdr();
    Lst l = (Lst) p.car();
    SpData sp1 = (SpData) sps.get(l.car());
    String sp1s = source.substring(sp1.getPos(), sp1.getEndPos());
    assertEquals("lambda", sp1s);
    
    l = (Lst) l.cdr();
    sp1 = (SpData) sps.get(l.car());
    sp1s = source.substring(sp1.getPos(), sp1.getEndPos());
    assertEquals("(a b c)", sp1s);
    
    Lst ll = (Lst) l.car();
    SpData sp2 = (SpData) sps.get(ll.car());
    String sp2s = source.substring(sp2.getPos(), sp2.getEndPos());
    assertEquals("a", sp2s);    
    
    ll = (Lst) ll.cdr();
    sp2 = (SpData) sps.get(ll.car());
    sp2s = source.substring(sp2.getPos(), sp2.getEndPos());
    assertEquals("b", sp2s);    
    
    ll = (Lst) ll.cdr();
    sp2 = (SpData) sps.get(ll.car());
    sp2s = source.substring(sp2.getPos(), sp2.getEndPos());
    assertEquals("c", sp2s);    
    
    l = (Lst) l.cdr();
    sp1 = (SpData) sps.get(l.car());
    sp1s = source.substring(sp1.getPos(), sp1.getEndPos());
    assertEquals("42.76", sp1s);    
  }

  public void testListWhiteSpace()
  {
    String source = "(lambda ( x ) #t)";
    Pair<Object, Map<Object, SpData>> p = doCorrectMatchingTest(source);
    Map<Object, SpData> sps = p.cdr();
    Lst l = (Lst) p.car();
    SpData sp1 = (SpData) sps.get(l.car());
    String sp1s = source.substring(sp1.getPos(), sp1.getEndPos());
    assertEquals("lambda", sp1s);
    
    l = (Lst) l.cdr();
    sp1 = (SpData) sps.get(l.car());
    sp1s = source.substring(sp1.getPos(), sp1.getEndPos());
    assertEquals("( x )", sp1s);
    
    Lst ll = (Lst) l.car();
    SpData sp2 = (SpData) sps.get(ll.car());
    String sp2s = source.substring(sp2.getPos(), sp2.getEndPos());
    assertEquals("x", sp2s);    
    
    l = (Lst) l.cdr();
    sp1 = (SpData) sps.get(l.car());
    sp1s = source.substring(sp1.getPos(), sp1.getEndPos());
    assertEquals("#t", sp1s);    
  }

  public void testVectorWhiteSpace()
  {
    String source = "(lambda #( x ) #t)";
    Pair<Object, Map<Object, SpData>> p = doCorrectMatchingTest(source);
    Map<Object, SpData> sps = p.cdr();
    Lst l = (Lst) p.car();
    SpData sp1 = (SpData) sps.get(l.car());
    String sp1s = source.substring(sp1.getPos(), sp1.getEndPos());
    assertEquals("lambda", sp1s);
    
    l = (Lst) l.cdr();
    sp1 = (SpData) sps.get(l.car());
    sp1s = source.substring(sp1.getPos(), sp1.getEndPos());
    assertEquals("#( x )", sp1s);
    
    Object[] ll = (Object[]) l.car();
    SpData sp2 = (SpData) sps.get(ll[0]);
    String sp2s = source.substring(sp2.getPos(), sp2.getEndPos());
    assertEquals("x", sp2s);    
    
    l = (Lst) l.cdr();
    sp1 = (SpData) sps.get(l.car());
    sp1s = source.substring(sp1.getPos(), sp1.getEndPos());
    assertEquals("#t", sp1s);    
  }

  public void testMinusRef()
  {
    String source = "(- n 4)";
    Pair<Object, Map<Object, SpData>> p = doCorrectMatchingTest(source);
    Map<Object, SpData> sps = p.cdr();
    Lst l = (Lst) p.car();
    SpData sp1 = (SpData) sps.get(l.car());
    String sp1s = source.substring(sp1.getPos(), sp1.getEndPos());
    assertEquals("-", sp1s);    
  }

  public Pair<Object, Map<Object, SpData>> doCorrectMatchingTest(String source)
  {
    SpParser2 parser = new SpParser2(source);
    Object data = parser.next();
    SpData spData = parser.getSps().get(data);
    assertNotNull(spData);
    int pos = spData.getPos();
    int endPos = spData.getEndPos();
    int linePos = spData.getLinePos();
    String substring = source.substring(pos, endPos);
    assertEquals(0, linePos);
    assertEquals(0, pos);
    assertEquals(source, substring);
    return Pair.cons(data, parser.getSps());
  }
  
  public void testLeadingTrailingSpaces()
  {
    String source = "  \t \n identifier   ";
    SpParser2 parser = new SpParser2(source);
    Object data = parser.next();
    Map<Object, SpData> sps = parser.getSps();
    SpData spData = sps.get(data);
    assertNotNull(spData);
    assertEquals(6, spData.getPos());
    assertEquals(10, spData.getLength());
    assertEquals(16, spData.getEndPos());
    assertEquals(1, spData.getLine());
    assertEquals(1, spData.getLinePos());
    assertEquals("  \t \n ", spData.getPrefix());
    assertEquals("identifier", source.substring(spData.getPos(), spData.getEndPos()));
  }
}
