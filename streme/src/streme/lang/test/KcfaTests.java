package streme.lang.test;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import junit.framework.TestCase;
import streme.lang.ast.AstDataCompiler;
import streme.lang.ast.Lambda;
import streme.lang.ast.Node;
import streme.lang.ast.analysis.kcfa.Kcfa;
import streme.lang.ast.analysis.kcfa.State;
import streme.lang.ast.analysis.kcfa.Store;
import streme.lang.ast.impl.StremeDataCompiler;
import streme.lang.data.Parser2;
import streme.lang.data.Sym;

public class KcfaTests extends TestCase
{
  
  public void testStandardExample()
  {
    String source = "((lambda (id) (id (lambda (z) z) (lambda (a) (id (lambda (y) y) (lambda (b) b))))) (lambda (x k) (k x)))";
    Parser2 parser = new Parser2();
    Object data = parser.parse(source);
    AstDataCompiler dataCompiler = new StremeDataCompiler();
    Node ast =  dataCompiler.compile(data);
    Kcfa cfa = new Kcfa(1);
    State initial = cfa.makeState(ast);
    Set<State> states = cfa.explore(initial);
    Store summary = cfa.summarize(states);
    Map<Sym, Set<Object>> monoStore = summary.monovariant();
    assertEquals(5, monoStore.size());
    checkLambdas(monoStore, "a", "[z]");
    checkLambdas(monoStore, "b", "[y]");
    checkLambdas(monoStore, "id", "[x, k]");
    checkLambdas(monoStore, "k", "[b]", "[a]");
    checkLambdas(monoStore, "x", "[z]", "[y]");
  }

  private void checkLambdas(Map<Sym, Set<Object>> monoStore, String var, String... lambdaSigs)
  {
    Set<Object> a = monoStore.get(new Sym(var));
    Set<String> as = new HashSet<String>();
    for (Object s : a)
    {
    as.add(Arrays.toString(((Lambda) s).getParams()));
    }
    assertEquals(lambdaSigs.length, as.size());
    for (String lambdaSig : lambdaSigs)
    {
      assertTrue(as.contains(lambdaSig));
    }
  }
}
