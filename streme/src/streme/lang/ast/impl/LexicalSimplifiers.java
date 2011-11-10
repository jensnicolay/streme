package streme.lang.ast.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import streme.lang.Logging;
import streme.lang.analysis.ParentAnalysis;
import streme.lang.analysis.ParentAnalyzer;
import streme.lang.analysis.VarPointerAnalysis;
import streme.lang.analysis.VarPointerAnalyzer;
import streme.lang.ast.AstVisitor;
import streme.lang.ast.Binding;
import streme.lang.ast.Let;
import streme.lang.ast.Node;
import streme.lang.ast.Node.Type;
import streme.lang.ast.Ref;
import streme.lang.ast.SetVar;
import streme.lang.ast.Var;
import streme.lang.ast.impl.RuleAstRewriter.Rule;
import streme.lang.data.Pair;
import streme.lang.data.Parser2;

public class LexicalSimplifiers
{
  public static final Logger LOGGER = Logger.getLogger("lexicalSimplifier");
  
  public static boolean comesAfter(Node context, final Node point, final Node queried)
  {
    final boolean[] result = new boolean[1];
    context.accept(new AstVisitor() {
      boolean pointEncountered;
      public boolean visitNode(Node node)
      {
        if (node.equals(point))
        {
          pointEncountered = true;
        }
        if (node.equals(queried))
        {
          result[0] = pointEncountered;
          return false;
        }
        return true;
      }
    });
    return result[0];
  }
  
  public static Node simplify(Node ast)
  {
    List<Rule> rules = new ArrayList<Rule>();
    rules.add(removeSrSwLetVar);
    RuleAstRewriter rar = new RuleAstRewriter(rules);
    Node post = ast;
    Node pre = null;
    while (!post.nodeEquals(pre))
    {
      pre = post;
      post = rar.rewrite(pre);
    }
    return post;
  }
  
//  public static Node remove(Node ast, final Node target)
//  {
//    List<Rule> rules = new ArrayList<Rule>();
//    rules.add(new Rule()
//    {
//      public boolean matches(Node node)
//      {
//        return node.equals(target);
//      }
//
//      public Pair<Boolean, Node> rewrite(Node node)
//      {
//        return rewrote(null);
//      }
//    });
//    return new RuleAstRewriter(rules).rewrite(ast);
//  }  
//  
  public static Node replace(Node ast, final Node target, final Node replacement)
  {
    List<Rule> rules = new ArrayList<Rule>();
    rules.add(new Rule()
    {
      public boolean matches(Node node)
      {
        return node.equals(target);
      }

      public Pair<Boolean, Node> rewrite(Node node)
      {
        return rewrote(replacement);
      }
    });
    return new RuleAstRewriter(rules).rewrite(ast);
  }  
  
  public static final RuleAstRewriter.Rule removeSrSwLetVar = new RuleAstRewriter.Rule()
  {
    public boolean matches(Node node)
    {
      if (node.type() != Type.LET)
      {
        return false;
      }
      Let let = (Let) node;
      Binding[] bindings = let.getBindings();
      if (bindings.length == 0)
      {
        return false;
      }
      boolean quotedOrNull = bindings[0].getValue() == null || bindings[0].getValue().type() == Type.LITERAL;
      return quotedOrNull;
    }

    public Pair<Boolean, Node> rewrite(Node node)
    {
      LOGGER.finer("attempting to rewrite " + node.toShortString());
      Let let = (Let) node;
      Binding[] bindings = let.getBindings();
      Var var = bindings[0].getVar();
      ParentAnalysis parentAnalysis = new ParentAnalyzer().analyze(let);
      VarPointerAnalysis varPointerAnalysis = new VarPointerAnalyzer(parentAnalysis).analyze(let);
      Set<Ref> readRefs = varPointerAnalysis.getReadRefs(var);
      Set<SetVar> writeRefs = varPointerAnalysis.getWriteRefs(var);
      if (readRefs != null && writeRefs != null)
      {
        if (readRefs.size() == 1 && writeRefs.size() == 1)
        {
          Node readRef = readRefs.iterator().next();
          SetVar writeRef = writeRefs.iterator().next();
          if (comesAfter(let, writeRef, readRef))
          {
            LOGGER.fine("bind x quoted, set! x v, ref x ==> v" + node.toShortString());
            Binding[] rbindings = Arrays.copyOfRange(let.getBindings(), 1, bindings.length);
            Node rlet = new Let(let.getKind(), rbindings, let.getBody());
            rlet = replace(rlet, writeRef, null);
            rlet = replace(rlet, readRef, writeRef.getValue());
            return rewrote(rlet);            
          }
        }
      }
      LOGGER.finer("no rewrite for " + node.toShortString());
      return unchanged(let);
    }
  };

  public static void main(String[] args)
  {
    Logging.setup(Level.FINE);
    String source = "(letrec ((_fib0 (lambda (_n1) (let ((_p2 (< _n1 2))) (if _p2 _n1 (let ((_p3 '<undefined>)) (let ((_p5 '<undefined>)) (let ((__p411 (future (let ((_p12 (- _n1 2))) (let ((_$9 (set! _p3 _p12))) (_fib0 _p3)))))) (let ((_p13 (- _n1 1))) (let ((_$10 (set! _p5 _p13))) (let ((_p6 (_fib0 _p5))) (let ((_p4 (touch __p411))) (+ _p4 _p6))))))))))))) (_fib0 30))";
    Parser2 parser = new Parser2();
    StremeDataCompiler compiler = new StremeDataCompiler();
    Node ast = compiler.compile(parser.parse(source));
//    ParentAnnotator.annotate(ast);
//    VarPointerAnnotator.annotate(ast);
//    List<Rule> rules = new ArrayList<Rule>();
//    rules.add(removeUnusedLetVar);
//    RuleAstRewriter rar = new RuleAstRewriter(rules);
//    System.out.println(rar.rewrite(ast));
    System.out.println(simplify(ast));
  }
}
