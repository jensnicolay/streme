package streme.lang.analysis;

import java.util.Set;

import streme.lang.ast.Application;
import streme.lang.ast.Var;

public interface IpdAnalysis
{
  Set<AbstractVar<Time>> getReads(Application application);
  Set<AbstractVar<Time>> getWrites(Application application);
  Set<Object> getValues(Var var);
  Set<Object> getMonoValues(Var var);
  Set<State> getResult();
}
