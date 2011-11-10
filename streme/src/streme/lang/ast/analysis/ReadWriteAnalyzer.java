package streme.lang.ast.analysis;

import java.util.ArrayList;
import java.util.List;

import streme.lang.ast.AstDataCompiler;
import streme.lang.ast.AstVisitor;
import streme.lang.ast.Lambda;
import streme.lang.ast.Node;
import streme.lang.ast.Ref;
import streme.lang.ast.SetVar;
import streme.lang.ast.impl.StremeDataCompiler;
import streme.lang.data.Parser2;
import streme.lang.data.Sym;

public class ReadWriteAnalyzer extends AstVisitor
{
    
  private List<Sym> reads;
  private List<Sym> writes;
  private boolean stepInsideLambdas;
  
  public ReadWriteAnalyzer(boolean stepInsideLambdas)
  {
    super();
    this.stepInsideLambdas = stepInsideLambdas;
    reads = new ArrayList<Sym>();
    writes = new ArrayList<Sym>();
  }

  public List<Sym> getReads()
  {
    return reads;
  }
  
  public List<Sym> getWrites()
  {
    return writes;
  }
  
//  public List<Pair<Sym, Access>> getReadWrites()
//  {
//    return readWrites;
//  }
  
  public void visitRef(Ref ref)
  {
    Sym sym = ref.getName();
    reads.add(sym);
  }

  public boolean visitSetVar(SetVar setVar)
  {
    Sym sym = setVar.getVar().getName();
    writes.add(sym);
    setVar.getValue().accept(this);
    return false;
  }
  
  public boolean visitLambda(Lambda lambda)
  {
    return stepInsideLambdas;
  }
  
  public static void main(String[] args)
  {
    String source = "(let ((ap1 'u) (ap2 'u) (ap3 'u) (ap4 'u)) (let ((bff0 (future (begin (set! ap1 (- n 1)) (set! ap2 (fib ap1))))) (bff1 (future (begin (set! ap3 (- n 2)) (set! ap4 (fib ap3)))))) (touch bff0) (touch bff1) (+ ap2 ap4)))";
    Parser2 parser = new Parser2();
    AstDataCompiler compiler = new StremeDataCompiler();
    Node ast = compiler.compile(parser.parse(source));
    ReadWriteAnalyzer la = new ReadWriteAnalyzer(true);
    ast.accept(la);
    System.out.println(la.getReads());
    System.out.println(la.getWrites());
  }
}
