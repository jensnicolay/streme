package streme.lang.eval.nd;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Callable;

import streme.lang.TCont;
import streme.lang.analysis.DependenceAnalyzer;
import streme.lang.analysis.IpdAnalysis;
import streme.lang.analysis.IpdAnalyzer;
import streme.lang.analysis.ParentAnalysis;
import streme.lang.analysis.ParentAnalyzer;
import streme.lang.analysis.VarPointerAnalysis;
import streme.lang.analysis.VarPointerAnalyzer;
import streme.lang.ast.Application;
import streme.lang.ast.AstDataCompiler;
import streme.lang.ast.AstVisitor;
import streme.lang.ast.Begin;
import streme.lang.ast.Binding;
import streme.lang.ast.Define;
import streme.lang.ast.If;
import streme.lang.ast.Lambda;
import streme.lang.ast.Let;
import streme.lang.ast.Literal;
import streme.lang.ast.Literal.Kind;
import streme.lang.ast.Node;
import streme.lang.ast.Ref;
import streme.lang.ast.SetVar;
import streme.lang.ast.Var;
import streme.lang.ast.impl.DataNodeUnifier;
import streme.lang.ast.impl.DataNodeUnifier.ConditionHandler;
import streme.lang.data.Lst;
import streme.lang.data.Parser2;
import streme.lang.data.Sym;
import streme.lang.eval.AstPrimitives;
import streme.lang.eval.MapEnv;

public class NdQueryPrimitives
{
  
  
  private static class Analysis
  {
    private Node ast;
    private ParentAnalysis parentAnalysis;
    private VarPointerAnalysis varPointerAnalysis;
    private IpdAnalysis ipdAnalysis;
    private DependenceAnalyzer dependenceAnalyzer;

    public Analysis(Node ast)
    {
      super();
      this.ast = ast;
      parentAnalysis = new ParentAnalyzer().analyze(ast);
      varPointerAnalysis = new VarPointerAnalyzer(parentAnalysis).analyze(ast);
      ipdAnalysis = new IpdAnalyzer(24, true, varPointerAnalysis).analyze(ast);
      dependenceAnalyzer = new DependenceAnalyzer(varPointerAnalysis, ipdAnalysis);
    }

    public Node getAst()
    {
      return ast;
    }

    public ParentAnalysis getParentAnalysis()
    {
      return parentAnalysis;
    }

    public VarPointerAnalysis getVarPointerAnalysis()
    {
      return varPointerAnalysis;
    }

    public IpdAnalysis getIpdAnalysis()
    {
      return ipdAnalysis;
    }

    public DependenceAnalyzer getDependenceAnalyzer()
    {
      return dependenceAnalyzer;
    }
  }

  private static final QEval QEVAL = new QEval();

  public static void loadSQueryPrimitives(MapEnv env)
  {
    env.add(new Sym("define-rule"), new Procedure()
    {
      public Callable<Callable> apply1(Object fact, final MapEnv env, final TSuccess cont, TCont fail)
      {
        env.add(fact, QEVAL.createRule(fact, null));
        return cont.call(Void.TYPE, fail);
      }

      public Callable<Callable> apply2(Object conclusion, Object body, final MapEnv env, final TSuccess cont, TCont fail)
      {
        env.add(conclusion, QEVAL.createRule(conclusion, body));
        return cont.call(Void.TYPE, fail);
      }
    });
    env.add(new Sym("query"), new Procedure()
    {
      public Callable<Callable> apply1(final Object q1, MapEnv env, final TSuccess cont, final TCont fail)
      {
        return QEVAL.qeval(q1, new LinkedHashMap(), env, new TSuccess()
        {
          public Callable<Callable> call(Object value, TCont fail)
          {
            return cont.call(QEVAL.resolve((Map) value, q1), fail);
          }
        }, fail);
      }
    });
  }

  public static void loadAstPrimitives(MapEnv env, final AstDataCompiler compiler)
  {
    env.add(new Sym("create-ast"), new Procedure()
    {
      public Callable<Callable> apply1(Object data, MapEnv env, TSuccess cont, TCont fail)
      {
        return cont.call(compiler.compile(data), fail);
      }
    });
    env.add(new Sym("analyze-ast"), new Procedure()
    {
      public Callable<Callable> apply1(Object astObject, MapEnv env, TSuccess cont, TCont fail)
      {
        Node ast = (Node) astObject;
        return cont.call(new Analysis(ast), fail);
      }
    });
    env.add(new Sym("match-quote"), new Procedure()
    {
      private Parser2 parser = new Parser2();
      private DataNodeUnifier unifier = new DataNodeUnifier();
      private ConditionHandler conditionHandler = new ConditionHandler()
      {
        public Map<Sym, Node> accept(Sym var, Node node, String condition)
        {
          Object q = parser.parse(condition);
          Map<Sym, Node> s = unifier.unify(q, node, this);
          if (s == null)
          {
            return null;
          }
          s.put(var, node);
          return s;
        }
      };

      public Callable<Callable> apply2(final Object pattern, final Object ast, final MapEnv env, final TSuccess cont,
          final TCont fail)
      {
        final List<Map<Sym, Node>> nodes = new ArrayList<Map<Sym, Node>>();
        ((Node) ast).accept(new AstVisitor()
        {
          public boolean visitNode(Node node)
          {
            Map<Sym, Node> unified = unifier.unify(pattern, node, conditionHandler);
            if (unified != null)
            {
              nodes.add(unified);
            }
            return true;
          }
        });
        if (nodes.isEmpty())
        {
          return fail.call(null);
        }
        Map<Sym, Node> bindings = nodes.get(0);
        for (Map.Entry<Sym, Node> binding : bindings.entrySet())
        {
          env.add(binding.getKey(), binding.getValue());
        }
        final int size = nodes.size();
        if (size == 1)
        {
          return cont.call(Void.TYPE, fail);
        }
        class BindingCont extends TCont
        {
          int i;

          BindingCont(int i)
          {
            super();
            this.i = i;
          }

          public Callable<Callable> call(Object value)
          {
            Map<Sym, Node> bindings = nodes.get(i);
            for (Map.Entry<Sym, Node> binding : bindings.entrySet())
            {
              env.add(binding.getKey(), binding.getValue());
            }
            return cont.call(Void.TYPE, i + 1 == size ? fail : new BindingCont(i + 1));
          }
        }
        return cont.call(Void.TYPE, new BindingCont(1));
      }
    });
    env.add(new Sym("name-equal?"), new Procedure()
    {
      public Callable<Callable> apply2(Object operand1, Object operand2, MapEnv env, TSuccess cont, TCont fail)
      {
        return cont.call(AstPrimitives.nameEqualp(operand1, operand2), fail);
      }
    });
    env.add(new Sym("var?"), new Procedure()
    {
      public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
      {
        return cont.call(AstPrimitives.varp(operand), fail);
      }
    });
    env.add(new Sym("create-var"), new Procedure()
    {
      public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
      {
        return cont.call(new Var(operand.toString()), fail);
      }
    });    
    env.add(new Sym("var-name"), new Procedure()
    {
      public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
      {
        return cont.call(AstPrimitives.varName(operand), fail);
      }
    });
    env.add(new Sym("ref?"), new Procedure()
    {
      public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
      {
        return cont.call(AstPrimitives.refp(operand), fail);
      }
    });
    env.add(new Sym("create-ref"), new Procedure()
    {
      public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
      {
        return cont.call(new Ref(operand.toString()), fail);
      }
    });
    env.add(new Sym("ref-name"), new Procedure()
    {
      public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
      {
        return cont.call(AstPrimitives.refName(operand), fail);
      }
    });
    env.add(new Sym("literal?"), new Procedure()
    {
      public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
      {
        return cont.call(AstPrimitives.literalp(operand), fail);
      }
    });
    env.add(new Sym("create-literal"), new Procedure()
    {
      public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
      {
        return cont.call(new Literal(operand, Kind.CONSTANT), fail);
      }
    });
    env.add(new Sym("lambda?"), new Procedure()
    {
      public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
      {
        return cont.call(AstPrimitives.lambdap(operand), fail);
      }
    });
    env.add(new Sym("create-lambda"), new Procedure()
    {
      public Callable<Callable> apply2(Object params, Object body, MapEnv env, TSuccess cont, TCont fail)
      {
        return cont.call(new Lambda(((Lst) params).properToArray(Var.class), (Node) body), fail);
      }
    });
    env.add(new Sym("lambda-params"), new Procedure()
    {
      public Callable<Callable> apply1(Object lambda, MapEnv env, TSuccess cont, TCont fail)
      {
        return cont.call(Lst.valueOf(((Lambda) lambda).getParams()), fail);
      }
    });
    env.add(new Sym("lambda-body"), new Procedure()
    {
      public Callable<Callable> apply1(Object lambda, MapEnv env, TSuccess cont, TCont fail)
      {
        return cont.call(((Lambda) lambda).getBody(), fail);
      }
    });
    env.add(new Sym("let?"), new Procedure()
    {
      public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
      {
        return cont.call(AstPrimitives.letp(operand), fail);
      }
    });
    env.add(new Sym("create-let"), new Procedure()
    {
      public Callable<Callable> apply2(Object bindings, Object body, MapEnv env, TSuccess cont, TCont fail)
      {
        return cont.call(new Let(Let.Kind.LET, ((Lst) bindings).properToArray(Binding.class), (Node) body), fail);
      }
    });
    env.add(new Sym("let-bindings"), new Procedure()
    {
      public Callable<Callable> apply1(Object let, MapEnv env, TSuccess cont, TCont fail)
      {
        return cont.call(Lst.valueOf(((Let) let).getBindings()), fail);
      }
    });
    env.add(new Sym("let-body"), new Procedure()
    {
      public Callable<Callable> apply1(Object let, MapEnv env, TSuccess cont, TCont fail)
      {
        return cont.call(((Let) let).getBody(), fail);
      }
    });
    env.add(new Sym("begin?"), new Procedure()
    {
      public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
      {
        return cont.call(AstPrimitives.beginp(operand), fail);
      }
    });
    env.add(new Sym("create-begin"), new Procedure()
    {
      public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
      {
        return cont.call(new Begin(((Lst) operand).properToArray(Node.class), Begin.Kind.EXPLICIT), fail);
      }
    });
    env.add(new Sym("begin-exps"), new Procedure()
    {
      public Callable<Callable> apply1(Object begin, MapEnv env, TSuccess cont, TCont fail)
      {
        return cont.call(Lst.valueOf(((Begin) begin).getExps()), fail);
      }
    });
    env.add(new Sym("define?"), new Procedure()
    {
      public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
      {
        return cont.call(AstPrimitives.definep(operand), fail);
      }
    });
    env.add(new Sym("create-define"), new Procedure()
    {
      public Callable<Callable> apply2(Object var, Object value, MapEnv env, TSuccess cont, TCont fail)
      {
        return cont.call(new Define((Var) var, (Node) value), fail);
      }
    });
    env.add(new Sym("define-var"), new Procedure()
    {
      public Callable<Callable> apply1(Object define, MapEnv env, TSuccess cont, TCont fail)
      {
        return cont.call(((Define) define).getVar(), fail);
      }
    });
    env.add(new Sym("define-value"), new Procedure()
    {
      public Callable<Callable> apply1(Object define, MapEnv env, TSuccess cont, TCont fail)
      {
        return cont.call(((Define) define).getValue(), fail);
      }
    });
    env.add(new Sym("set?"), new Procedure()
    {
      public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
      {
        return cont.call(AstPrimitives.setvarp(operand), fail);
      }
    });
    env.add(new Sym("create-set"), new Procedure()
    {
      public Callable<Callable> apply2(Object var, Object value, MapEnv env, TSuccess cont, TCont fail)
      {
        return cont.call(new SetVar((Var) var, (Node) value), fail);
      }
    });
    env.add(new Sym("set-var"), new Procedure()
    {
      public Callable<Callable> apply1(Object setVar, MapEnv env, TSuccess cont, TCont fail)
      {
        return cont.call(((SetVar) setVar).getVar(), fail);
      }
    });
    env.add(new Sym("set-value"), new Procedure()
    {
      public Callable<Callable> apply1(Object setVar, MapEnv env, TSuccess cont, TCont fail)
      {
        return cont.call(((SetVar) setVar).getValue(), fail);
      }
    });
    env.add(new Sym("application?"), new Procedure()
    {
      public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
      {
        return cont.call(AstPrimitives.applicationp(operand), fail);
      }
    });
    env.add(new Sym("create-application"), new Procedure()
    {
      public Callable<Callable> apply2(Object operator, Object operands, MapEnv env, TSuccess cont, TCont fail)
      {
        return cont.call(new Application((Node) operator, ((Lst) operands).properToArray(Node.class)), fail);
      }
    });
    env.add(new Sym("application-operator"), new Procedure()
    {
      public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
      {
        return cont.call(AstPrimitives.applicationOperator(operand), fail);
      }
    });
    env.add(new Sym("application-operands"), new Procedure()
    {
      public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
      {
        return cont.call(AstPrimitives.applicationOperands(operand), fail);
      }
    });
    env.add(new Sym("application-operand"), new Procedure()
    {
      public Callable<Callable> apply2(Object operand, Object index, MapEnv env, TSuccess cont, TCont fail)
      {
        return cont.call(AstPrimitives.applicationOperand(operand, index), fail);
      }
    });
    env.add(new Sym("binding?"), new Procedure()
    {
      public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
      {
        return cont.call(AstPrimitives.bindingp(operand), fail);
      }
    });
    env.add(new Sym("create-binding"), new Procedure()
    {
      public Callable<Callable> apply2(Object var, Object value, MapEnv env, TSuccess cont, TCont fail)
      {
        return cont.call(new Binding((Var) var, (Node) value), fail);
      }
    });
    env.add(new Sym("binding-var"), new Procedure()
    {
      public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
      {
        return cont.call(AstPrimitives.bindingVar(operand), fail);
      }
    });
    env.add(new Sym("binding-value"), new Procedure()
    {
      public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
      {
        return cont.call(AstPrimitives.bindingValue(operand), fail);
      }
    });
    env.add(new Sym("if?"), new Procedure()
    {
      public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
      {
        return cont.call(AstPrimitives.ifp(operand), fail);
      }
    });
    env.add(new Sym("create-if"), new Procedure()
    {
      public Callable<Callable> apply3(Object condition, Object consequent, Object alternate, MapEnv env, TSuccess cont, TCont fail)
      {
        return cont.call(new If((Node) condition, (Node) consequent, (Node) alternate), fail);
      }
    });
    env.add(new Sym("if-condition"), new Procedure()
    {
      public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
      {
        return cont.call(((If) operand).getCondition(), fail);
      }
    });
    env.add(new Sym("if-consequent"), new Procedure()
    {
      public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
      {
        return cont.call(((If) operand).getConsequent(), fail);
      }
    });
    env.add(new Sym("if-alternate"), new Procedure()
    {
      public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
      {
        return cont.call(((If) operand).getAlternate(), fail);
      }
    });
  }

  public static void loadParentAnalysisPrimitives(MapEnv env, final ParentAnalysis parentAnalysis)
  {
    env.add(new Sym("parent"), new Procedure()
    {
      public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
      {
        return cont.call(parentAnalysis.getParent((Node) operand), fail);
      }
    });
  }

  private NdQueryPrimitives()
  {
    super();
  }
}
