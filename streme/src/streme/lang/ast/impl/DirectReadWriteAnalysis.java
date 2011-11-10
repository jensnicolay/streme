package streme.lang.ast.impl;

import java.util.Set;

import streme.lang.ast.Var;

public interface DirectReadWriteAnalysis
{
  Set<Var> getReads();
  Set<Var> getWrites();
}
