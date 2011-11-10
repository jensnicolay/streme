package streme.lang;

@SuppressWarnings("serial")
public class StremeException extends RuntimeException
{

  public StremeException(String message)
  {
    super(message);
  }

  public StremeException(String message, Throwable cause)
  {
    super(message, cause);
  }
}
