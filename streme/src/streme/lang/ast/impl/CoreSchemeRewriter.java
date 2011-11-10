package streme.lang.ast.impl;

import streme.lang.StremeException;
import streme.lang.ast.Application;
import streme.lang.ast.AstDataCompiler;
import streme.lang.ast.AstRewriter;
import streme.lang.ast.Begin;
import streme.lang.ast.Binding;
import streme.lang.ast.Define;
import streme.lang.ast.Future;
import streme.lang.ast.If;
import streme.lang.ast.Lambda;
import streme.lang.ast.Let;
import streme.lang.ast.Literal;
import streme.lang.ast.Node;
import streme.lang.ast.Ref;
import streme.lang.ast.SetVar;
import streme.lang.ast.Var;
import streme.lang.ast.analysis.RenamingStrategy;
import streme.lang.data.Parser2;
import streme.lang.data.Sym;

public class CoreSchemeRewriter implements AstRewriter
{
  private static final RenamingStrategy renamer = RenamingStrategy.NUMBER_RENAMING_STRATEGY;
  private static final Ref TOUCH = new Ref(new Sym("touch"));
  
  public Node rewrite(Node node)
  {
    switch (node.type())
    {
      case APPLICATION:
        Application application = (Application) node;
        return new Application(rewrite(application.getOperator()), rewrite(application.getOperands()));
      case BEGIN:
        Begin begin = (Begin) node;
        return new Begin(rewrite(begin.getExps()), Begin.Kind.EXPLICIT);
      case DEFINE:
        Define define = (Define) node;
        return new Define(define.getVar(), rewrite(define.getValue()));
      case FUTURE:
        Future future = (Future) node;
        return new Future(rewrite(future.getValue()));
      case IF:
        If iff = (If) node;
        return new If(rewrite(iff.getCondition()), rewrite(iff.getConsequent()), rewrite(iff.getAlternate()));
      case LAMBDA:
        Lambda lambda = (Lambda) node;
        return new Lambda(lambda.getParams(), rewrite(lambda.getBody()), lambda.getVarparam());
      case LET:
        Let let = (Let) node;
        Binding[] bindings = let.getBindings();
        int numBindings = bindings.length;
        switch (let.getKind())
        {
          case LET:
            Var[] params = new Var[numBindings];
            Node[] operands = new Node[numBindings];
            for (int i = 0; i < numBindings; i++)
            {
              params[i] = bindings[i].getVar();
              operands[i] = rewrite(bindings[i].getValue());
            }
            return new Application(new Lambda(params, rewrite(let.getBody())), operands);
          case LETREC:
            Binding[] nullBindings = new Binding[numBindings];
            Node[] setters = new Node[numBindings];
            for (int i = 0; i < numBindings; i++)
            {
              nullBindings[i] = new Binding(bindings[i].getVar(), Literal.UNDEFINED);
              setters[i] = new SetVar(bindings[i].getVar(), bindings[i].getValue());
            }
            return rewrite(new Let(Let.Kind.LET, nullBindings, new Begin(setters, Begin.Kind.EXPLICIT).append(let.getBody())));
          case LETSTAR:
            Node result = let.getBody();
            for (int i = numBindings - 1; i > -1; i--)
            {
              result = new Let(Let.Kind.LET, let.getBindings()[i], result);
            }
            return rewrite(result);
//          case LETPAR:
//            Binding[] futureBindings = new Binding[numBindings];
//            Binding[] touchBindings = new Binding[numBindings];
//            for (int i = 0; i < numBindings; i++)
//            {
//              Sym name = bindings[i].getVar().getName();
//              Sym rname = renamer.rename(name);
//              futureBindings[i] = new Binding(new Var(rname), new Future(bindings[i].getValue()));
//              touchBindings[i] = new Binding(new Var(name), new Application(TOUCH, new Ref(rname)));
//            }
//            return rewrite(new Let(Let.Kind.LET, futureBindings, new Let(Let.Kind.LET, touchBindings, let.getBody())));
          default:
          {
            throw new StremeException("cannot handle " + let);
          }
        }
      case SETVAR:
        SetVar setVar = (SetVar) node;
        return new SetVar(setVar.getVar(), rewrite(setVar.getValue()));
      default:
        return node;
    }
  }

  private Node[] rewrite(Node[] exps)
  {
    Node[] rewritten = new Node[exps.length];
    for (int i = 0; i < exps.length; i++)
    {
      rewritten[i] = rewrite(exps[i]);
    }
    return rewritten;
  }

  public static void main(String[] args)
  {
    CoreSchemeRewriter csr = new CoreSchemeRewriter();
    Parser2 parser = new Parser2();
    AstDataCompiler compiler = new StremeDataCompiler();
    String source = "(letrec ((a 2) (b (+ a 5))) (* a b))";
    System.out.println(csr.rewrite(compiler.compile(parser.parse(source))));
  }
}
