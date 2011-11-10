package streme.lang.analysis;

import streme.lang.StremeException;
import streme.lang.ast.AstVisitor;
import streme.lang.ast.Binding;
import streme.lang.ast.Define;
import streme.lang.ast.Lambda;
import streme.lang.ast.Let;
import streme.lang.ast.Node;
import streme.lang.ast.Node.Type;
import streme.lang.ast.Var;
import streme.lang.ast.impl.StremeSpDataCompiler2;
import streme.lang.data.SpParser2;
import streme.lang.eval.nd.NdAnalysisStreme;

public class LexicalParentWalker
{
  public interface Acceptor
  {
    boolean accept(Var var);
  }

  private ParentAnalysis parentAnalysis;

  public LexicalParentWalker(ParentAnalysis parentAnalysis)
  {
    super();
    this.parentAnalysis = parentAnalysis;
  }

  public void visit(Node current, final Acceptor acceptor)
  {
    Node parent = parentAnalysis.getParent(current);
    while (parent != null)
    {
      switch (parent.type())
      {
        case LET:
        {
          Let parentLet = (Let) parent;
          if (current.type() == Type.BINDING)
          {
            Let.Kind kind = parentLet.getKind();
            if (kind == Let.Kind.LET || kind == Let.Kind.LETPAR)
            {
              // no other binding visible
            }
            else if (kind == Let.Kind.LETSTAR)
            {
              Binding[] bindings = parentLet.getBindings();
              int i;
              for (i = 0; i < bindings.length; i++)
              {
                if (bindings[i].nodeEquals(current))
                {
                  for (int j = i - 1; j > -1; j--)
                  {
                    if (!acceptor.accept(bindings[j].getVar()))
                    {
                      return;
                    }
                  }
                }
              }
            }
            else if (kind == Let.Kind.LETREC)
            {
              // all binding vars are visible
              Binding[] bindings = parentLet.getBindings();
              for (Binding binding : bindings)
              {
                if (!acceptor.accept(binding.getVar()))
                {
                  return;
                }
              }
            }
            else
            {
              throw new StremeException("cannot handle let kind " + parentLet.toShortString());
            }
          }
          else
          {
            Binding[] bindings = parentLet.getBindings();
            for (Binding binding : bindings)
            {
              if (!acceptor.accept(binding.getVar()))
              {
                return;
              }
            }
          }
          break;
        }
        case DEFINE:
        {
          Define parentDefine = (Define) parent;
          if (!acceptor.accept(parentDefine.getVar()))
          {
            return;
          }
          break;
        }
        case LAMBDA:
        {
          Lambda parentLambda = (Lambda) parent;
          Var[] params = parentLambda.getParams();
          for (Var var : params)
          {
            if (!acceptor.accept(var))
            {
              return;
            }
          }
        }
      }
      current = parent;
      parent = parentAnalysis.getParent(current);
    }
    final boolean[] varHolder = new boolean[] { true};
    current.accept(new AstVisitor()
    {
      public boolean visitDefine(Define define)
      {
        if (varHolder[0] && !acceptor.accept(define.getVar()))
        {
          varHolder[0] = false;
          return false;
        }
        return true;
      }
    });
  }

  public static void main(String[] args)
  {
    SpParser2 parser = new SpParser2("(let ((a 1)) (let ((b 2)) (let ((a 3)) (+ a b))))");
    Object data = parser.next();
    StremeSpDataCompiler2 compiler = new StremeSpDataCompiler2(parser.getSps());
    Node ast = compiler.compile(data);
    ParentAnalysis pa = new ParentAnalyzer().analyze(ast);
    LexicalParentWalker lpw = new LexicalParentWalker(pa);
    NdAnalysisStreme querier = new NdAnalysisStreme(ast);
    Node node = querier.queryNode("(an-application)");
    System.out.println(new VarPointerAnalyzer(pa).inScope(node));
  }
}
