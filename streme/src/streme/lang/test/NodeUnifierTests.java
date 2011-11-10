package streme.lang.test;

import java.util.Map;
import java.util.TreeMap;

import junit.framework.TestCase;
import streme.lang.ast.AstDataCompiler;
import streme.lang.ast.Node;
import streme.lang.ast.impl.NodeUnifier;
import streme.lang.ast.impl.StremeDataCompiler;
import streme.lang.data.Parser2;
import streme.lang.data.Sym;

public class NodeUnifierTests extends TestCase
{
  private Parser2 parser = new Parser2();
  private AstDataCompiler compiler = new StremeDataCompiler();
  private NodeUnifier unifier = new NodeUnifier();

  private void unify(String n1str, String n2str, String expected)
  {
    Node n1 = compiler.compile(parser.parse(n1str));
    Node n2 = compiler.compile(parser.parse(n2str));
    Map<Sym, Node> subs = unifier.unify(n1, n2, null);
    String result = (subs == null ? null : new TreeMap<Sym, Object>(subs).toString());
    assertEquals(expected, result);
  }

//  private void unifyRewrite(String n1str, String n2str, String n3str, String expected)
//  {
//    Object n1 = parser.parse(n1str);
//    Object n2 = parser.parse(n2str);
//    Object n3 = parser.parse(n3str);
//    Map<Sym, Object> subs = unifier.unify(n1, n2);
//    Object result = unifier.apply(subs, n3, false).toString();
//    assertEquals(expected, result);
//  }

  public void testApplication()
  {
    unify("(f ?x1 b)", "(f a b)", "{?x1=a}");
    unify("(f a ?x1)", "(f a b)", "{?x1=b}");
    unify("(?x1 a b)", "(f a b)", "{?x1=f}");
    unify("(?x0 ?x1 ?x2)", "(f a b)", "{?x0=f, ?x1=a, ?x2=b}");
    unify("?x", "(f a b)", "{?x=(f a b)}");
    unify("(?x1 (g a ?x2) c)", "(f (g a b) c)", "{?x1=f, ?x2=b}");
  }
  
  public void testLambda()
  {
    unify("(lambda (?x1) body)", "(lambda (p) body)", "{?x1=p}");
    unify("(lambda (?x1) body)", "(lambda (p) otherbody)", null);
  }
  
  public void testLists()
  {
    unify("(?x ?y ?z)", "(1 2 3)", "{?x=1, ?y=2, ?z=3}");
    unify("(?x ?y ?z)", "(1 2 3 4)", null);
//    unify("(?x ?y . ?z)", "(1 2 3 4)", "{?x=1, ?y=2, ?z=(3 4)}");
    unify("((?x) ?x)", "((1) 1)", "{?x=1}");
    unify("((?x) ?x)", "((1) 2)", null);
    unify("((?x) ?x)", "((2) 1)", null);
    unify("(((?x)) ?x)", "(((1)) 1)", "{?x=1}");
    unify("(((?x)) ?x)", "(((1)) 2)", null);
    unify("(((?x)) ?x)", "(((2)) 1)", null);
    unify("(((?x)) (?x))", "(((1)) (1))", "{?x=1}");
    unify("(((?x)) (?x))", "(((1)) (2))", null);
    unify("(((?x)) (?x))", "(((2)) (1))", null);
  }
  
  public void testAny()
  {
    unify("(?x ?y ?)", "(1 2 3)", "{?x=1, ?y=2}");
    unify("(?x ?y ?)", "(1 2 3 4)", null);
//    unify("(?x ?y . ?)", "(1 2 3 4)", "{?x=1, ?y=2}");
  }
  
//  public void testSplice()
//  {
//    unifyRewrite("(synchronized ?lock . ?exps)", "(synchronized l exp1 exp2 exp3)", "((lock ?lock) @?exps (unlock ?lock))", "((lock l) exp1 exp2 exp3 (unlock l))");
//  }
}
