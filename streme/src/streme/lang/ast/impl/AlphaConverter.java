package streme.lang.ast.impl;

import streme.lang.StremeException;
import streme.lang.ast.AstDataCompiler;
import streme.lang.ast.Binding;
import streme.lang.ast.Lambda;
import streme.lang.ast.Let;
import streme.lang.ast.Node;
import streme.lang.ast.Ref;
import streme.lang.ast.SetVar;
import streme.lang.ast.Var;
import streme.lang.ast.analysis.RenamingStrategy;
import streme.lang.data.Lst;
import streme.lang.data.Null;
import streme.lang.data.Pair;
import streme.lang.data.Parser2;
import streme.lang.data.Sym;

public class AlphaConverter extends RecursiveDescentAstRewriter
{
  private RenamingStrategy renamingStrategy;
  private Lst subs;

  public AlphaConverter(RenamingStrategy renamingStrategy)
  {
    super();
    this.renamingStrategy = renamingStrategy;
    subs = new Null();
  }

  protected Node rewriteRef(Ref ref)
  {
    Sym name = ref.getName();
    Object replace = subs.assoc(name);
    if (Boolean.FALSE.equals(replace))
    {
      return ref;
    }
    Ref newRef = new Ref((Sym) ((Pair) replace).cdr());
    return newRef;
  }

  protected Node rewriteLambda(Lambda lambda)
  {
    Lst restoreSubs = subs;
    Var[] params = lambda.getParams();
    Var[] newParams = new Var[params.length];
    for (int i = 0; i < params.length; i++)
    {
      Sym name = params[i].getName();
      Sym newName = renamingStrategy.rename(name);
      newParams[i] = new Var(newName);
      subs = Pair.cons(Pair.cons(name, newName), subs);
    }
    Var varparam = lambda.getVarparam();
    Var newVarparam;
    if (varparam == null)
    {
      newVarparam = null;
    }
    else
    {
      Sym name = varparam.getName();
      Sym newName = renamingStrategy.rename(name);
      newVarparam = new Var(newName);
      subs = Pair.cons(Pair.cons(name, newName), subs);
    }
    Lambda newLambda = new Lambda(newParams, rewrite(lambda.getBody()), newVarparam);
    subs = restoreSubs;
    return newLambda;
  }

  protected Node rewriteLet(Let let)
  {
    Lst restoreSubs = subs;
    Binding[] bindings = let.getBindings();
    switch (let.getKind())
    {
      case LET:
      {
        Binding[] newBindings = new Binding[bindings.length];
        Lst bindingSubs = subs;
        for (int i = 0; i < bindings.length; i++)
        {
          Var var = bindings[i].getVar();
          Sym name = var.getName();
          Sym newName = renamingStrategy.rename(name);
          bindingSubs = Pair.cons(Pair.cons(name, newName), bindingSubs);
          Var newVar = new Var(newName);
          newBindings[i] = new Binding(newVar, rewrite(bindings[i].getValue()));
        }
        subs = bindingSubs;
        Let newLet = new Let(Let.Kind.LET, newBindings, rewrite(let.getBody()));
        subs = restoreSubs;
        return newLet;
      }
      case LETREC:
      {
        Binding[] newBindings = new Binding[bindings.length];
        Lst bindingSubs = subs;
        for (int i = 0; i < bindings.length; i++)
        {
          Var var = bindings[i].getVar();
          Sym name = var.getName();
          Sym newName = renamingStrategy.rename(name);
          bindingSubs = Pair.cons(Pair.cons(name, newName), bindingSubs);
          Var newVar = new Var(newName);
          newBindings[i] = new Binding(newVar, null);
        }
        subs = bindingSubs;
        for (int i = 0; i < bindings.length; i++)
        {
          newBindings[i].setValue(rewrite(bindings[i].getValue()));
        }
        Let newLet = new Let(Let.Kind.LETREC, newBindings, rewrite(let.getBody()));
        subs = restoreSubs;
        return newLet;
      }        
      default:
        throw new StremeException("cannot handle " + let);
    }
  }
  
  protected Node rewriteSetVar(SetVar setVar)
  {
    Var var = setVar.getVar();
    Sym name = var.getName();
    Object replace = subs.assoc(name);
    Node value = setVar.getValue();
    if (Boolean.FALSE.equals(replace))
    {
      return new SetVar(var, rewrite(value));
    }
    Var newVar = new Var((Sym) ((Pair) replace).cdr());
    return new SetVar(newVar, rewrite(value));
  }

  public static void main(String[] args)
  {
    Parser2 parser = new Parser2();
    AstDataCompiler compiler = new StremeDataCompiler();
    String source = " (let ((x '())) (set! x (cons 123 x)))";
    Object data = parser.parse(source);
    Node ast = compiler.compile(data);
    AlphaConverter renamer = new AlphaConverter(RenamingStrategy.NUMBER_RENAMING_STRATEGY);
    System.out.println(renamer.rewrite(ast));
  }
}
