package streme.lang.ast.analysis.ipda;

import java.util.List;
import java.util.Set;

import streme.lang.ast.analysis.kcfa.Addr;
import streme.lang.ast.analysis.kcfa.Store;

public interface IState
{
  Store getStore();
  List<IState> next();
  Set<Addr> reads();
  Set <Addr> writes();
  Set<Kont> konts();
  IState gc();
  int getNumber();
  //boolean subsumes(IState state);
  Object getContext();
  void join(IState state);
}
