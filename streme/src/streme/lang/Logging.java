package streme.lang;

import java.io.OutputStream;
import java.util.logging.ConsoleHandler;
import java.util.logging.Formatter;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;

public class Logging
{
  static public void setup()
  {
    setup(Level.INFO);
  }

  
  static public void setup(Level level)
  {
    // Create Logger
    Logger logger = Logger.getLogger("");
    for (Handler h : logger.getHandlers())
    {
      logger.removeHandler(h);      
    }
    logger.setLevel(level);
    ConsoleHandler consoleHandler = new ConsoleHandler()
    {
      protected synchronized void setOutputStream(OutputStream out) throws SecurityException
      {
        super.setOutputStream(System.out);
      }
    };
    logger.addHandler(consoleHandler);
    consoleHandler.setFormatter(new Formatter()
    {
      public String format(LogRecord record)
      {
        String sourceClassName = record.getSourceClassName();
        return sourceClassName.substring(sourceClassName.lastIndexOf('.') + 1) + "." + record.getSourceMethodName() + ": " + record.getMessage() + "\n";
      }
    });
    consoleHandler.setLevel(Level.ALL);
  }
}
