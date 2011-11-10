package streme.lang.data;

import streme.lang.StremeException;

public class StremeSpException extends StremeException
{
  
  private int pos;
  private int line;
  private int linePos;
  private int length;
  
  public StremeSpException(String message, int pos, int line, int linePos, int length)
  {
    super(message);
    this.pos = pos;
    this.line = line;
    this.linePos = linePos;
    this.length = length;
  }

  public int getPos()
  {
    return pos;
  }

  public int getLine()
  {
    return line;
  }

  public int getLinePos()
  {
    return linePos;
  }

  public int getLength()
  {
    return length;
  }
  
  
}
