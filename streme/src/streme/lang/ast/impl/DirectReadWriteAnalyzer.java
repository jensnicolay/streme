package streme.lang.ast.impl;

import java.util.HashSet;
import java.util.Set;

import streme.lang.StremeException;
import streme.lang.analysis.ParentAnalysis;
import streme.lang.analysis.ParentAnalyzer;
import streme.lang.analysis.Primitives;
import streme.lang.analysis.VarPointerAnalysis;
import streme.lang.analysis.VarPointerAnalyzer;
import streme.lang.ast.AstAnalyzer;
import streme.lang.ast.AstDataCompiler;
import streme.lang.ast.AstVisitor;
import streme.lang.ast.Lambda;
import streme.lang.ast.Node;
import streme.lang.ast.Ref;
import streme.lang.ast.SetVar;
import streme.lang.ast.Var;
import streme.lang.data.Parser2;

/**
 * Collects all variables directly read and written. "Directly" means immediately derivable from the source code, as
 * determined by VarPointerVisitor. This excludes variables read and written as a consequence of procedure invocation
 * for example. Depends on prior traversal by {@link VarPointerAnalyzer}.
 */
public class DirectReadWriteAnalyzer implements AstAnalyzer<DirectReadWriteAnalysis>
{
  
  private VarPointerAnalysis varPointerAnalysis;
  private boolean stepInsideLambdas;

  public DirectReadWriteAnalyzer(VarPointerAnalysis varPointerAnalysis, boolean stepInsideLambdas)
  {
    super();
    this.varPointerAnalysis = varPointerAnalysis;
    this.stepInsideLambdas = stepInsideLambdas;
  }
  
  public DirectReadWriteAnalysis analyze(Node node)
  {
    final Set<Var> reads = new HashSet<Var>();
    final Set<Var> writes = new HashSet<Var>();
    node.accept(new AstVisitor() {
      public void visitRef(Ref ref)
      {
        Var readVar = varPointerAnalysis.getVarRead(ref);
        if (readVar == null)
        {
          Object prim = Primitives.get(ref.getName());
          if (prim == null)
          {
            throw new StremeException("cannot get var for ref " + ref);
          }
          else
          {
            // nothing (?)
          }
        }
        else
        {
          reads.add(readVar);
        }
      }

      public boolean visitSetVar(SetVar setVar)
      {
        Var writtenVar = varPointerAnalysis.getVarWritten(setVar);
        if (writtenVar == null)
        {
          throw new StremeException("cannot get var for setVar " + setVar);
        }
        writes.add(writtenVar);
        setVar.getValue().accept(this);
        return false;
      }

      public boolean visitLambda(Lambda lambda)
      {
        return stepInsideLambdas;
      }
      
    });
    return new DirectReadWriteAnalysis()
    {
      public Set<Var> getWrites()
      {
        return writes;
      }
      
      public Set<Var> getReads()
      {
        return reads;
      }
    };
  }


  public static void main(String[] args)
  {
    String source = "(define fib (lambda (n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))))";
    Parser2 parser = new Parser2();
    AstDataCompiler compiler = new StremeDataCompiler();
    Node ast = compiler.compile(parser.parse(source));
    ParentAnalysis parentAnalysis = new ParentAnalyzer().analyze(ast);
    VarPointerAnalysis varPointerAnalysis = new VarPointerAnalyzer(parentAnalysis).analyze(ast);
    DirectReadWriteAnalysis drwa = new DirectReadWriteAnalyzer(varPointerAnalysis, true).analyze(ast);
    System.out.println(drwa.getReads());
    System.out.println(drwa.getWrites());
  }
}
