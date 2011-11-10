package streme.lang.data;


public class SpData
{
  private String prefix;
  private String suffix;
  private int pos;
  private int line;
  private int linePos;
  private int length;
  
  private String original;

  public SpData(String prefix, int pos, int line, int linePos, int length, String suffix)
  {
    super();
    this.prefix = prefix;
    this.suffix = suffix;
    this.pos = pos;
    this.line = line;
    this.linePos = linePos;
    this.length = length;
  }


  public int getLine()
  {
    return line;
  }

  public int getLinePos()
  {
    return linePos;
  }


  public int getPos()
  {
    return pos;
  }
  
  public int getEndPos()
  {
    return pos + length;
  }


  public int getLength()
  {
    return length;
  }
  
  public String getPrefix()
  {
    return prefix;
  }
  
  public String getSuffix()
  {
    return suffix;
  }

  public String toString()
  {
    return "{pos " + pos + " length " + length + " prefix |" + prefix + "| suffix |" + suffix + "|}";
  }


  public String getOriginal()
  {
    return original;
  }


  public void setOriginal(String original)
  {
    this.original = original;
  }
}