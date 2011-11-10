package streme.lang.ast.analysis;

import java.util.Arrays;

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
import streme.lang.ast.Let.Kind;
import streme.lang.ast.Literal;
import streme.lang.ast.Node;
import streme.lang.ast.Node.Type;
import streme.lang.ast.Ref;
import streme.lang.ast.SetVar;
import streme.lang.ast.Var;
import streme.lang.ast.impl.AlphaConverter;
import streme.lang.ast.impl.StremeDataCompiler;
import streme.lang.data.Lst;
import streme.lang.data.Null;
import streme.lang.data.Pair;
import streme.lang.data.Parser2;
import streme.lang.data.Sym;

/**
 * Requires unique variable names, provided for example by {@link AlphaConverter}.
 */
public class AnfConverter implements AstRewriter
{
  private interface Continuation
  {
    Node receive(Node node);
  }

  private RenamingStrategy renamingStrategy;

  public AnfConverter(RenamingStrategy renamingStrategy)
  {
    super();
    this.renamingStrategy = renamingStrategy;
  }

  public Node rewrite(Node node)
  {
    return rewrite(node, new Continuation()
    {
      public Node receive(Node node)
      {
        return node;
      }
    });
  }

  private Node rewrite(Node node, final Continuation k)
  {
    switch (node.type())
    {
      case LAMBDA:
        Lambda lambda = (Lambda) node;
        return k.receive(new Lambda(lambda.getParams(), rewrite(lambda.getBody()), lambda.getVarparam()));
      case LET:
      {
        final Let let = (Let) node;
        final Kind kind = let.getKind();
        Binding[] bindings = let.getBindings();
        int numBindings = bindings.length;
        if (numBindings == 1)
        {
          return rewrite(let.getValue(0), new Continuation()
          {
            public Node receive(Node node)
            {
              return new Let(kind == Let.Kind.LETSTAR ? Let.Kind.LET : kind, let.getName(0), node, rewrite(
                  let.getBody(), k));
            }
          });
        }
        else if (numBindings == 0)
        {
          return k.receive(rewrite(let.getBody()));
        }
        if (kind == Let.Kind.LET || kind == Let.Kind.LETSTAR)
        {
          Node r = new Let(Let.Kind.LET, bindings[numBindings - 1].getVar(), bindings[numBindings - 1].getValue(),
              let.getBody());
          for (int i = numBindings - 2; i > -1; i--)
          {
            r = new Let(Let.Kind.LET, bindings[i].getVar(), bindings[i].getValue(), r);
          }
          return rewrite(r, k);
        }
        throw new StremeException("cannot handle " + let);
      }
      case SETVAR:
        final SetVar setVar = (SetVar) node;
        return rewriteName(setVar.getValue(), new Continuation()
        {
          public Node receive(Node node)
          {
            return k.receive(new SetVar(setVar.getVar(), node));
          }
        });
      case IF:
        final If iff = (If) node;
        return rewriteName(iff.getCondition(), new Continuation()
        {
          public Node receive(Node node)
          {
            return k.receive(new If(node, rewrite(iff.getConsequent()), rewrite(iff.getAlternate())));
          }
        });
      case APPLICATION:
        final Application application = (Application) node;
        return rewriteName(application.getOperator(), new Continuation()
        {
          public Node receive(final Node operator)
          {
            return rewriteNames(Lst.valueOf(application.getOperands()), new Continuation()
            {
              public Node receive(Node tt)
              {
                Lst ott = (Lst) ((Literal) tt).getValue();
                Node[] operands = ott.properToArray(Node.class);
                int arity = operands.length;
                if (operator.type() == Type.LAMBDA && arity > 0)
                {
                  Lambda lam = (Lambda) operator;
                  Node r = new Let(Let.Kind.LET, lam.getParams()[arity - 1], operands[arity - 1], lam.getBody());
                  for (int i = arity - 2; i > -1; i--)
                  {
                    r = new Let(Let.Kind.LET, lam.getParams()[i], operands[i], r);
                  }
                  return k.receive(r);
                }
                return k.receive(new Application(operator, operands));
              }
            });
          }
        });
      case DEFINE:
        final Define define = (Define) node;
        return rewriteName(define.getValue(), new Continuation()
        {
          public Node receive(Node node)
          {
            return k.receive(new Define(define.getVar(), node));
          }
        });
      case BEGIN:
        Begin begin = (Begin) node;
        final Node[] exps = begin.getExps();
        Node headExp = exps[0];
        if (exps.length == 1)
        {
          return k.receive(rewrite(headExp));
        }
        return rewrite(headExp, new Continuation()
        {
          public Node receive(Node node)
          {
            Sym temp = renamingStrategy.rename(new Sym("$"));
            if (exps.length == 1)
            {
              return k.receive(new Let(Let.Kind.LET, new Var(temp), node, new Ref(temp)));
            }
            else
            {
              return new Let(Let.Kind.LET, new Var(temp), node, rewrite(
                  new Begin(Arrays.copyOfRange(exps, 1, exps.length), Begin.Kind.EXPLICIT), k));
            }
          }
        });
      case FUTURE:
      {
        Future future = (Future) node;
        return k.receive(new Future(rewrite(future.getValue())));
      }
      default:
        return k.receive(node);
    }
  }

  private Node rewriteName(Node node, final Continuation k)
  {
    return rewrite(node, new Continuation()
    {
      public Node receive(Node node)
      {
        if (simple(node))
        {
          return k.receive(node);
        }
        else
        {
          Sym temp = renamingStrategy.rename(new Sym("p"));
          return new Let(Let.Kind.LET, new Var(temp), node, k.receive(new Ref(temp)));
        }
      }
    });
  }

  private Node rewriteNames(final Lst lst, final Continuation k)
  {
    if (lst.isNull())
    {
      return k.receive(new Literal(lst, Literal.Kind.DATUM));
    }
    return rewriteName((Node) lst.car(), new Continuation()
    {
      public Node receive(final Node t)
      {
        return rewriteNames((Lst) lst.cdr(), new Continuation()
        {
          public Node receive(Node tt)
          {
            Lst ott = (Lst) ((Literal) tt).getValue();
            return k.receive(new Literal(Pair.cons(t, ott), Literal.Kind.DATUM));
          }
        });
      }
    });
  }

  private boolean simple(Node node)
  {
    Type type = node.type();
    return type == Type.LITERAL || type == Type.LAMBDA || type == Type.REF;
  }

  public static void main(String[] args)
  {
    Parser2 parser = new Parser2();
    AstDataCompiler compiler = new StremeDataCompiler();
    AnfConverter anfConverter = new AnfConverter(RenamingStrategy.NUMBER_RENAMING_STRATEGY);
    String source = "(let ((f (lambda () f))) 'hello)";
    Node ast = compiler.compile(parser.parse(source));
    System.out.println("ast: " + ast);
    Node anf = anfConverter.rewrite(ast);
    System.out.println("anf: " + anf);
    // List<Node> astQueries = NodeFinders.findUnifyingNodes(ast, parser.parse("z"));
    // for (Node astQuery : astQueries)
    // {
    // System.out.println("ast query: " + astQuery + " (" + astQuery.type() + ")");
    // System.out.println("==> " + AnfConverter.findSymbol(anf, astQuery));
    // }
  }
}
