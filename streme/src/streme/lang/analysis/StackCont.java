package streme.lang.analysis;

import java.util.LinkedHashSet;
import java.util.Set;

import streme.lang.DefaultCont;
import streme.lang.ast.Application;

public abstract class StackCont extends DefaultCont<State>
{

  private final StackCont previous;
  private final BindingEnv bindingEnv;
  private final Set<Application> applicationMarks;
  
  
  public StackCont(String name, StackCont previous, BindingEnv bindingEnv)
  {
    super(name);
    this.previous = previous;
    this.bindingEnv = bindingEnv;
    //marks = new LinkedHashSet<AbstractProcedure<Application>>();
    applicationMarks = new LinkedHashSet<Application>();
  }
  
//  public void mark(AbstractProcedure<Application> mark)
//  {
//    marks.add(mark);
//  }
  
  public void markApplication(Application mark)
  {
    applicationMarks.add(mark);
  }
  
//  public Set<AbstractProcedure<Application>> getMarks()
//  {
//    return marks;
//  }
  
  public Set<Application> getApplicationMarks()
  {
    return applicationMarks;
  }
  
  public StackCont getPrevious()
  {
    return previous;
  }
  
  public BindingEnv getBindingEnv()
  {
    return bindingEnv;
  }
  
  public String toString()
  {
    return getName() + (applicationMarks.isEmpty() ? "" : applicationMarks) + " -> " + previous;
  }
}
