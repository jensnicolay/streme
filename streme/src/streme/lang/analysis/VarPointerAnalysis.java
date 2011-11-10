package streme.lang.analysis;

import java.util.List;
import java.util.Set;

import streme.lang.ast.Node;
import streme.lang.ast.Ref;
import streme.lang.ast.SetVar;
import streme.lang.ast.Var;

public interface VarPointerAnalysis
{
  Var getVarRead(Ref ref);  
  Var getVarWritten(SetVar setVar);
  Set<Ref> getReadRefs(Var var);
  Set<SetVar> getWriteRefs(Var var);
  List<Var> inScope(Node node);
}
