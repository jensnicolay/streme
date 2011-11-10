package streme.lang.ast.analysis;

import java.util.concurrent.atomic.AtomicInteger;

import streme.lang.data.Sym;

public abstract class RenamingStrategy
{
  
  private static class NumberRenamingStrategy extends RenamingStrategy
  {
    private static final AtomicInteger counter = new AtomicInteger();
    private String prefix;
    
    private NumberRenamingStrategy(String prefix)
    {
      super();
      this.prefix = prefix;
    }
    
    public Sym rename(Sym original)
    {
      return new Sym(prefix + original + counter.getAndIncrement());
    }
  }
  
  public static final RenamingStrategy NUMBER_RENAMING_STRATEGY = new NumberRenamingStrategy("_");
  public static final RenamingStrategy METAVAR_RENAMING_STRATEGY = new NumberRenamingStrategy("?");
  
  
  public abstract Sym rename(Sym original);
}