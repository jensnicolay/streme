package streme.lang.analysis;

import java.util.HashSet;
import java.util.Set;

import streme.lang.ast.Application;
import streme.lang.ast.AstAnalyzer;
import streme.lang.ast.AstDataCompiler;
import streme.lang.ast.AstVisitor;
import streme.lang.ast.Lambda;
import streme.lang.ast.Node;
import streme.lang.ast.impl.StremeDataCompiler;
import streme.lang.data.Parser2;

/**
 * Collects all abstract variables read and written as a consequence of procedure invocation.
 */
public class IpdReadWriteAnalyzer implements AstAnalyzer<IpdReadWriteAnalysis>
{
  private IpdAnalysis ipdAnalysis;

  public IpdReadWriteAnalyzer(IpdAnalysis ipdAnalysis)
  {
    super();
    this.ipdAnalysis = ipdAnalysis;
  }

  public IpdReadWriteAnalysis analyze(Node node)
  {
    final Set<AbstractVar<Time>> reads = new HashSet<AbstractVar<Time>>();
    final Set<AbstractVar<Time>> writes = new HashSet<AbstractVar<Time>>();
    node.accept(new AstVisitor()
    {
      public boolean visitApplication(Application application)
      {
        reads.addAll(ipdAnalysis.getReads(application));
        writes.addAll(ipdAnalysis.getWrites(application));
        return true;
      }

      public boolean visitLambda(Lambda lambda)
      {
        // don't step into lambda's
        return false;
      }
    });
    return new IpdReadWriteAnalysis()
    {
      public Set<AbstractVar<Time>> getWrites()
      {
        return writes;
      }

      public Set<AbstractVar<Time>> getReads()
      {
        return reads;
      }
    };
  }

  public static void main(String[] args)
  {
    String source = "(let* ((z 0) (writez (lambda () (set! z 123)))) (writez))";
    Parser2 parser = new Parser2();
    AstDataCompiler compiler = new StremeDataCompiler();
    Node ast = compiler.compile(parser.parse(source));
    ParentAnalysis parentAnalysis = new ParentAnalyzer().analyze(ast);
    VarPointerAnalysis varPointerAnalysis = new VarPointerAnalyzer(parentAnalysis).analyze(ast);
    IpdAnalysis ipdAnalysis = new IpdAnalyzer(12, true, varPointerAnalysis).analyze(ast);
    IpdReadWriteAnalysis ipdReadWriteAnalysis = new IpdReadWriteAnalyzer(ipdAnalysis).analyze(ast);
    System.out.println(ipdReadWriteAnalysis.getReads());
    System.out.println(ipdReadWriteAnalysis.getWrites());
  }
}
