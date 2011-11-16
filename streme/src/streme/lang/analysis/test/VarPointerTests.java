package streme.lang.analysis.test;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import junit.framework.TestCase;
import streme.lang.analysis.ParentAnalysis;
import streme.lang.analysis.ParentAnalyzer;
import streme.lang.analysis.VarPointerAnalysis;
import streme.lang.analysis.VarPointerAnalyzer;
import streme.lang.ast.Node;
import streme.lang.ast.Ref;
import streme.lang.ast.Var;
import streme.lang.ast.impl.StremeDataCompiler;
import streme.lang.data.Parser2;
import streme.lang.eval.nd.NdAnalysisStreme;

/*
 * TODO: test readRefs and writeRefs
 */
public class VarPointerTests extends TestCase
{
  private Parser2 parser = new Parser2();
  private StremeDataCompiler compiler = new StremeDataCompiler();
  private ParentAnalyzer parentAnalyzer = new ParentAnalyzer();

  private Set<Object> setOf(Object... el)
  {
    Set<Object> s = new HashSet<Object>();
    for (Object e : el)
    {
      s.add(e);
    }
    return s;
  }

  private VarPointerAnalysis doAnalysis(Node ast)
  {
    ParentAnalysis parentAnalysis = parentAnalyzer.analyze(ast);
    VarPointerAnalyzer varPointerAnalyzer = new VarPointerAnalyzer(parentAnalysis);
    VarPointerAnalysis varPointerAnalysis = varPointerAnalyzer.analyze(ast);
    return varPointerAnalysis;
  }

  private Node createAst(String source)
  {
    Node ast = compiler.compile(parser.parse(source));
    return ast;
  }

  public void testLet1()
  {
    String source = "(let ((a 1)) a)";
    Node ast = createAst(source);
    VarPointerAnalysis analysis = doAnalysis(ast);
    NdAnalysisStreme querier = new NdAnalysisStreme(ast);
    Var avar = (Var) querier.queryNode("(a-var-with-name 'a)");
    Ref aref = (Ref) querier.queryNode("(a-ref-with-name 'a)");
    assertEquals(setOf(aref), analysis.getReadRefs(avar));
    assertEquals(setOf(), analysis.getWriteRefs(avar));
    assertEquals(avar, analysis.getVarRead(aref));
    querier.shutdown();
  }

//  public void testLet2()
//  {
//    String source = "(let ((a 1)) b)";
//    Node ast = createAst(source);
//    VarPointerAnalysis analysis = doAnalysis(ast);
//    NdNodeQuerier querier = new NdNodeQuerier(ast);
//    Var avar = (Var) querier.queryNode("(a-var-with-name 'a)");
//    Ref bref = (Ref) querier.queryNode("(a-ref-with-name 'b)");
//    assertEquals(setOf(), analysis.getReadRefs(avar));
//    assertEquals(setOf(), analysis.getWriteRefs(avar));
//    assertEquals(null, analysis.getVarRead(bref)); NOT ALLOWED: undefd ref
//    querier.shutdown();
//  }

  public void testLet3()
  {
    String source = "(let ((a 1) (b 2)) b)";
    Node ast = createAst(source);
    VarPointerAnalysis analysis = doAnalysis(ast);
    NdAnalysisStreme querier = new NdAnalysisStreme(ast);
    Var avar = (Var) querier.queryNode("(a-var-with-name 'a)");
    Var bvar = (Var) querier.queryNode("(a-var-with-name 'b)");
    Ref bref = (Ref) querier.queryNode("(a-ref-with-name 'b)");
    assertEquals(setOf(), analysis.getReadRefs(avar));
    assertEquals(setOf(), analysis.getWriteRefs(avar));
    assertEquals(setOf(bref), analysis.getReadRefs(bvar));
    assertEquals(setOf(), analysis.getWriteRefs(bvar));
    assertEquals(bvar, analysis.getVarRead(bref));
    querier.shutdown();
  }

//  public void testLet4()
//  {
//    String source = "(let ((a 1) (b a)) c)";
//    Node ast = createAst(source);
//    VarPointerAnalysis analysis = doAnalysis(ast);
//    NdNodeQuerier querier = new NdNodeQuerier(ast);
//    Var avar = (Var) querier.queryNode("(a-var-with-name 'a)");
//    Ref aref = (Ref) querier.queryNode("(a-ref-with-name 'a)");
//    Var bvar = (Var) querier.queryNode("(a-var-with-name 'b)");
//    Ref cref = (Ref) querier.queryNode("(a-ref-with-name 'c)");
//    assertEquals(setOf(), analysis.getReadRefs(avar));
//    assertEquals(setOf(), analysis.getWriteRefs(avar));
//    assertEquals(setOf(), analysis.getReadRefs(bvar));
//    assertEquals(setOf(), analysis.getWriteRefs(bvar));
//    assertEquals(null, analysis.getVarRead(aref)); NOT ALLOWED
//    assertEquals(null, analysis.getVarRead(cref));
//    querier.shutdown();
//  }

  public void testLet5()
  {
    String source = "(let* ((a 1) (b a)) b)";
    Node ast = createAst(source);
    VarPointerAnalysis analysis = doAnalysis(ast);
    NdAnalysisStreme querier = new NdAnalysisStreme(ast);
    Var avar = (Var) querier.queryNode("(a-var-with-name 'a)");
    Ref aref = (Ref) querier.queryNode("(a-ref-with-name 'a)");
    Var bvar = (Var) querier.queryNode("(a-var-with-name 'b)");
    Ref bref = (Ref) querier.queryNode("(a-ref-with-name 'b)");
    assertEquals(setOf(aref), analysis.getReadRefs(avar));
    assertEquals(setOf(), analysis.getWriteRefs(avar));
    assertEquals(setOf(bref), analysis.getReadRefs(bvar));
    assertEquals(setOf(), analysis.getWriteRefs(bvar));
    assertEquals(avar, analysis.getVarRead(aref));
    assertEquals(bvar, analysis.getVarRead(bref));
    querier.shutdown();
  }

  public void testLet6()
  {
    String source = "(letrec ((a a) (b a)) b)";
    Node ast = createAst(source);
    VarPointerAnalysis analysis = doAnalysis(ast);
    NdAnalysisStreme querier = new NdAnalysisStreme(ast);
    Var avar = (Var) querier.queryNode("(a-var-with-name 'a)");
    Var bvar = (Var) querier.queryNode("(a-var-with-name 'b)");
    List<Ref> arefs = querier.query("(a-ref-with-name 'a)", Ref.class);
    assertEquals(2, arefs.size());
    Ref aref1 = arefs.get(0);
    Ref aref2 = arefs.get(1);
    Ref bref = (Ref) querier.queryNode("(a-ref-with-name 'b)");
    assertEquals(setOf(aref1, aref2), analysis.getReadRefs(avar));
    assertEquals(setOf(), analysis.getWriteRefs(avar));
    assertEquals(setOf(bref), analysis.getReadRefs(bvar));
    assertEquals(setOf(), analysis.getWriteRefs(bvar));
    assertEquals(avar, analysis.getVarRead(aref1));
    assertEquals(avar, analysis.getVarRead(aref2));
    assertEquals(bvar, analysis.getVarRead(bref));
    querier.shutdown();
  }

  public void testDefine1()
  {
    String source = "(define t (lambda () t))";
    Node ast = createAst(source);
    VarPointerAnalysis analysis = doAnalysis(ast);
    NdAnalysisStreme querier = new NdAnalysisStreme(ast);
    Var tvar = (Var) querier.queryNode("(a-var-with-name 't)");
    Ref tref = (Ref) querier.queryNode("(a-ref-with-name 't)");
    assertEquals(setOf(tref), analysis.getReadRefs(tvar));
    assertEquals(setOf(), analysis.getWriteRefs(tvar));
    assertEquals(tvar, analysis.getVarRead(tref));
    querier.shutdown();
  }
  
  public void testDefine2()
  {
    String source = "(begin (define x 123) (define y x))";
    Node ast = createAst(source);
    VarPointerAnalysis analysis = doAnalysis(ast);
    NdAnalysisStreme querier = new NdAnalysisStreme(ast);
    Var xvar = (Var) querier.queryNode("(a-var-with-name 'x)");
    Ref xref = (Ref) querier.queryNode("(a-ref-with-name 'x)");
    assertEquals(setOf(xref), analysis.getReadRefs(xvar));
    assertEquals(xvar, analysis.getVarRead(xref));
    querier.shutdown();
  }
}
