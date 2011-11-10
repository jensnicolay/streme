package streme.lang.ast;

public class AstVisitor
{
  public boolean visitNode(Node node)
  {
    return true;
  }
  
  public void endVisitNode(Node node)
  {
  }

  public boolean visitApplication(Application application)
  {
    return visitNode(application);
  }

  public void endVisitApplication(Application application)
  {
    endVisitNode(application);
  }

  public boolean visitIf(If ff)
  {
    return visitNode(ff);
  }

  public void endVisitIf(If ff)
  {
    endVisitNode(ff);
  }

  public boolean visitBegin(Begin begin)
  {
    return visitNode(begin);
  }

  public void endVisitBegin(Begin begin)
  {
    endVisitNode(begin);
  }

  public boolean visitDefine(Define define)
  {
    return visitNode(define);
  }

  public void endVisitDefine(Define define)
  {
    endVisitNode(define);
  }

  public boolean visitFuture(Future future)
  {
    return visitNode(future);
  }

  public void endVisitFuture(Future future)
  {
    endVisitNode(future);
  }

  public boolean visitLambda(Lambda lambda)
  {
    return visitNode(lambda);
  }

  public void endVisitLambda(Lambda lambda)
  {
    endVisitNode(lambda);
  }

  public void visitVar(Var var)
  {
    visitNode(var);
  }

  public void visitRef(Ref ref)
  {
    visitNode(ref);
  }

  public void visitLiteral(Literal literal)
  {
    visitNode(literal);
  }

  public boolean visitSetVar(SetVar setVar)
  {
    return visitNode(setVar);
  }

  public void endVisitSetVar(SetVar setVar)
  {
    endVisitNode(setVar);
  }
  
  public boolean visitLet(Let let)
  {
    return visitNode(let);
  }

  public void endVisitLet(Let let)
  {
    endVisitNode(let);
  }
  
  public boolean visitBinding(Binding binding)
  {
    return visitNode(binding);
  }

  public void endVisitBinding(Binding binding)
  {
    endVisitNode(binding);
  }

}
