package streme.lang;

import java.util.List;
import java.util.concurrent.AbstractExecutorService;
import java.util.concurrent.TimeUnit;

public class DirectExecutor extends AbstractExecutorService
{

  private volatile boolean running;
  
  public void shutdown()
  {
    running = false;
  }

  public List<Runnable> shutdownNow()
  {
    running = false;
    return null;
  }

  public boolean isShutdown()
  {
    return !running;
  }

  public boolean isTerminated()
  {
    return !running;
  }

  public boolean awaitTermination(long timeout, TimeUnit unit) throws InterruptedException
  {
    throw new UnsupportedOperationException();
  }

  public void execute(Runnable command)
  {
    if (running)
    {
      command.run();
    }
  }
  
}
