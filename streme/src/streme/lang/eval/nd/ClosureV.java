package streme.lang.eval.nd;

import java.util.concurrent.Callable;

import streme.lang.StremeException;
import streme.lang.TCont;
import streme.lang.ast.Node;
import streme.lang.data.Lst;
import streme.lang.data.Null;
import streme.lang.data.Pair;
import streme.lang.data.Sym;
import streme.lang.eval.MapEnv;

public class ClosureV extends Procedure
{
  private MapEnv staticEnv;
  private Sym[] params;
  private Sym varparam;
  private Exp body;
  
  public ClosureV(Sym[] params, Sym varparam, Exp body, MapEnv env)
  {
    super();
    this.params = params;
    this.varparam = varparam;
    this.body = body;
    this.staticEnv = env;
  }

  public Callable<Callable> apply0(MapEnv env, TSuccess cont, TCont fail)
  { 
    if (params.length == 0)
    {
      MapEnv extended = new MapEnv(staticEnv);
      extended.add(new Sym("dynamic-env"), env);
      extended.add(varparam, new Null());
      return body.eval(extended, cont, fail);
    }
    else
    {
      throw new StremeException("wrong number of arguments (0) for " + this);
    }
  }
  
  public Callable<Callable> apply1(Object operand, MapEnv env, TSuccess cont, TCont fail)
  {
    MapEnv extended = new MapEnv(staticEnv);
    extended.add(new Sym("dynamic-env"), env);
    switch (params.length)
    {
      case 0:
        extended.add(varparam, Pair.cons(operand, new Null()));
        break;
      case 1:
        extended.add(params[0], operand);
        extended.add(varparam, new Null());
        break;
      default:
        throw new StremeException("wrong number of arguments (1) for " + this);
    }
    return body.eval(extended, cont, fail);
  }
  
  public Callable<Callable> apply2(Object operand1, Object operand2, MapEnv env, TSuccess cont, TCont fail)
  {
    MapEnv extended = new MapEnv(staticEnv);
    extended.add(new Sym("dynamic-env"), env);
    switch (params.length)
    {
      case 0:
        extended.add(varparam, Pair.cons(operand1, Pair.cons(operand2, new Null())));
        break;
      case 1:
        extended.add(params[0], operand1);
        extended.add(varparam, Pair.cons(operand2, new Null()));
        break;
      case 2:
        extended.add(params[0], operand1);
        extended.add(params[1], operand2);
        extended.add(varparam, new Null());
        break;
      default:
        throw new StremeException("wrong number of arguments (2) for " + this);
    }
    return body.eval(extended, cont, fail);
  }
  
  public Callable<Callable> apply3(Object operand1, Object operand2, Object operand3, MapEnv env, TSuccess cont, TCont fail)
  {
    MapEnv extended = new MapEnv(staticEnv);
    extended.add(new Sym("dynamic-env"), env);
    switch (params.length)
    {
      case 0:
        extended.add(varparam, Pair.cons(operand1, Pair.cons(operand2, Pair.cons(operand3, new Null()))));
        break;
      case 1:
        extended.add(params[0], operand1);
        extended.add(varparam, Pair.cons(operand2, Pair.cons(operand3, new Null())));
        break;
      case 2:
        extended.add(params[0], operand1);
        extended.add(params[1], operand2);
        extended.add(varparam, Pair.cons(operand3, new Null()));
        break;
      case 3:
        extended.add(params[0], operand1);
        extended.add(params[1], operand2);
        extended.add(params[2], operand3);
        extended.add(varparam, new Null());
        break;
      default:
        throw new StremeException("wrong number of arguments (3) for " + this);
    }
    return body.eval(extended, cont, fail);
  }
  
  public Callable<Callable> applyN(Object[] operands, MapEnv env, TSuccess cont, TCont fail)
  {
    MapEnv extended = new MapEnv(staticEnv);
    extended.add(new Sym("dynamic-env"), env);
    for (int i = 0; i < params.length; i++)
    {
      extended.add(params[i], operands[i]);
    }
    Lst l = new Null();
    for (int i = operands.length - 1; params.length <= i; i--)
    {
      l = Pair.cons(operands[i], l);
    }
    extended.add(varparam, l);
    return body.eval(extended, cont, fail);
  } 
  
}
