package streme.lang.test;

import java.util.ArrayList;
import java.util.List;

import junit.framework.TestCase;
import streme.lang.ast.AstDataCompiler;
import streme.lang.ast.Node;
import streme.lang.ast.impl.LexicalSimplifiers;
import streme.lang.ast.impl.RuleAstRewriter;
import streme.lang.ast.impl.RuleAstRewriter.Rule;
import streme.lang.ast.impl.StremeDataCompiler;
import streme.lang.data.Parser2;

public class LexicalSimplifiersTests extends TestCase
{
  private Parser2 parser = new Parser2();
  private AstDataCompiler compiler = new StremeDataCompiler();

  private Node createAst(String node)
  {
    Node ast = compiler.compile(parser.parse(node));
    return ast;
  }
  
//  public void testRemoveUnusedLetVars1()
//  {
//    String source = "(let (($26 (f 1 2 3))) 'something)";
//    Node ast = createAst(source);
//    List<Rule> rules = new ArrayList<Rule>();
//    rules.add(LexicalSimplifiers.removeSrSwLetVar);
//    RuleAstRewriter rar = new RuleAstRewriter(rules);
//    Node rewritten = rar.rewrite(ast);
//    assertEquals("(begin (f 1 2 3) something)", rewritten.toString());
//  }

//  public void testRemoveUnusedLetVars2()
//  {
//    String source = "(let (($26 (f 1 $26 3))) 'something)";
//    Node ast = createAst(source);
//    List<Rule> rules = new ArrayList<Rule>();
//    rules.add(LexicalSimplifiers.removeSrSwLetVar);
//    RuleAstRewriter rar = new RuleAstRewriter(rules);
//    Node rewritten = rar.rewrite(ast);
//    assertEquals("(begin (f 1 $26 3) something)", rewritten.toString());
//  }

//  public void testRemoveUnusedLetVars3()
//  {
//    String source = "(let* (($26 (f 1 $26 3))) 'something)";
//    Node ast = createAst(source);
//    List<Rule> rules = new ArrayList<Rule>();
//    rules.add(LexicalSimplifiers.removeSrSwLetVar);
//    RuleAstRewriter rar = new RuleAstRewriter(rules);
//    Node rewritten = rar.rewrite(ast);
//    assertEquals("(begin (f 1 $26 3) something)", rewritten.toString());
//  }


  public void testRemoveUnusedLetVars4()
  {
    String source = "(letrec (($26 (f 1 $26 3))) 'something)";
    Node ast = createAst(source);
    List<Rule> rules = new ArrayList<Rule>();
    rules.add(LexicalSimplifiers.removeSrSwLetVar);
    RuleAstRewriter rar = new RuleAstRewriter(rules);
    Node rewritten = rar.rewrite(ast);
    assertEquals("(letrec (($26 (f 1 $26 3))) something)", rewritten.toString());
  }

  public void testRemoveUnusedLetVars5()
  {
    String source = "(let () 'something)";
    Node ast = createAst(source);
    List<Rule> rules = new ArrayList<Rule>();
    rules.add(LexicalSimplifiers.removeSrSwLetVar);
    RuleAstRewriter rar = new RuleAstRewriter(rules);
    Node rewritten = rar.rewrite(ast);
    assertEquals("something", rewritten.toString());
  }
  
//  public void testRemoveUnusedLetVars6()
//  {
//    String source = "(let ((_pivot0 '<undefined>)) (set! _pivot0 'lambda) _pivot0)";
//    Node ast = createAst(source);
//    List<Rule> rules = new ArrayList<Rule>();
//    rules.add(LexicalSimplifiers.removeSrSwLetVar);
//    RuleAstRewriter rar = new RuleAstRewriter(rules);
//    Node rewritten = rar.rewrite(ast);
//    assertEquals("(let ((_pivot0 lambda)) _pivot0)", rewritten.toString());
//  }
  

//  public void testLet()
//  {
//    simplify("(let ((x 1)) x)", "1");
//    simplify("(let ((x 1)) (+ x y))", "(+ 1 y)");
//    simplify("(let ((x 1) (y 3)) (+ x y))", "(+ 1 3)");
//  }
  
}
