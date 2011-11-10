package streme.lang.analysis;

import java.util.Collections;
import java.util.Set;

import streme.lang.ast.Application;
import streme.lang.ast.AstAnalyzer;
import streme.lang.ast.Node;
import streme.lang.ast.Var;
import streme.lang.ast.impl.StremeDataCompiler;
import streme.lang.ast.impl.TagPrinter;
import streme.lang.data.Parser2;
import streme.lang.eval.nd.NdAnalysisStreme;

public class IpdAnalyzer implements AstAnalyzer<IpdAnalysis>
{
  
  private int k;
  private boolean gc;
  private VarPointerAnalysis varPointerAnalysis;
  
  public IpdAnalyzer(int k, boolean gc, VarPointerAnalysis varPointerAnalysis)
  {
    super();
    this.k = k;
    this.gc = gc;
    this.varPointerAnalysis = varPointerAnalysis;
  }
  
  public IpdAnalysis analyze(Node node)
  {
    final StatefulIpdAnalyzer ipda = new StatefulIpdAnalyzer(k, gc, varPointerAnalysis);
    ipda.analyze(node);
    return new IpdAnalysis()
    {
      public Set<AbstractVar<Time>> getWrites(Application application)
      {
        Set<AbstractVar<Time>> r = ipda.getWrites().get(application);
        if (r == null)
        {
          return Collections.emptySet();
        }
        return r;
      }
      
      public Set<AbstractVar<Time>> getReads(Application application)
      {
        Set<AbstractVar<Time>> r = ipda.getReads().get(application);
        if (r == null)
        {
          return Collections.emptySet();
        }
        return r;
      }
      
      public Set<State> getResult()
      {
        return ipda.getResult();
      }
      
      public Set<Object> getValues(Var var)
      {
        return ipda.getValues(var);
      }  
      
      public Set<Object> getMonoValues(Var var)
      {
        return ipda.getMonoValues(var);
      }
    };
  }
  

  public static void main(String[] args)
  {
     String source = "(let ((l (list + - * /))) ((car l) 1 2))";
     //String source = "(begin (define z 123) (define f (lambda () z)) (f) (f))";
    // String source = "(begin (define counter (lambda (c) (if (zero? c) 'boem (counter (- c 1))))) (counter 2))";
//    String source = "(letrec ((fib (lambda (n) (if (< n 2) n (let ((a (fib (- n 1))) (b (fib (- n 2)))) (+ a b)))))) (fib 3))";
    //String source = "(let* ((z 0) (writez (lambda () (set! z 123))) (readz (lambda () z))) (cons (writez) (readz)))";
    //String source = "(let* ((z 0) (writez (lambda () (set! z 123)))) (writez))";
    Parser2 parser = new Parser2();
    Object data = parser.parse(source);
    StremeDataCompiler compiler = new StremeDataCompiler();
    Node ast = compiler.compile(data);
    NdAnalysisStreme querier = new NdAnalysisStreme(ast);
    System.out.println(querier.query("(ipd-result)"));
    querier.shutdown();
  }

}
