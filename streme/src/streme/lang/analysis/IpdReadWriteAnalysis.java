package streme.lang.analysis;

import java.util.Set;

public interface IpdReadWriteAnalysis
{
  Set<AbstractVar<Time>> getReads();
  Set<AbstractVar<Time>> getWrites();
}
