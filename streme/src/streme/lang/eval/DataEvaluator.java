package streme.lang.eval;


public interface DataEvaluator<E>
{
  E globalEnv();
  Object evaluateData(Object data, E env);
}
