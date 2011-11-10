package streme.lang.data;

import java.io.IOException;
import java.io.PushbackReader;
import java.io.Reader;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.List;

import streme.lang.StremeException;

public class Parser2
{
  private static final Object[] EMPTY_VECTOR = new Object[0];

  public Object parse(CharSequence chars)
  {
    return parse(new StringReader(chars.toString()));
  }

  public Object parse(Reader reader)
  {
    try
    {
      int c = skipWhiteSpace(reader);
      return parse(c, new PushbackReader(reader));
    }
    catch (Exception e)
    {
      throw new StremeException("error while parsing", e);
    }
  }

  private Object parse(int c, PushbackReader reader) throws IOException
  {
    switch (c)
    {
      case '(':
        return parseList(reader);
      case '\'':
        return parseQuote(reader);
      case '`':
        return parseQuasiquote(reader);
      case ',':
        return parseUnquote(reader);
      case '"':
        return parseString(reader);
      case '#':
      {
        int d = reader.read();
        if (d == 't')
        {
          return Boolean.TRUE;
        }
        if (d == 'f')
        {
          return Boolean.FALSE;
        }
        if (d == '(')
        {
          return parseVector(reader);
        }
        throw new StremeException("illegal syntax: #" + (char) d);
      }
      case '-':
      {
        int d = reader.read();
        if (Character.isWhitespace(d) || d == ')')
        {
          reader.unread(d);
          return new Sym("-");
        }
        reader.unread(d);
        return parseNumber(c, reader);
      }
      case -1:
        return null;
    }
    if (Character.isDigit(c))
    {
      return parseNumber(c, reader);
    }
    return parseIdentifier(c, reader);
  }

  private Sym parseIdentifier(int c, PushbackReader reader) throws IOException
  {
    StringBuilder sb = new StringBuilder();
    sb.append((char) c);
    while (!Character.isWhitespace(c = reader.read()) && c != ')' && c != -1)
    {
      sb.append((char) c);
      if (c == '{')
      {
        parseCurly(sb, reader);
      }
    }
    if (c == ')')
    {
      reader.unread(c);
    }
    String s = sb.toString();
    return new Sym(s);
  }

  private void parseCurly(StringBuilder sb, Reader reader) throws IOException
  {
    int c;
    while ((c = reader.read()) != '}')
    {
      if (c == -1)
      {
        throw new StremeException("unmatched '{'");
      }
      sb.append((char) c);
    }
    sb.append('}');
  }

  private Object parseQuote(PushbackReader reader) throws IOException
  {
    int c = reader.read();
    Object e = parse(c, reader);
    return Pair.cons(new Sym("quote"), Pair.cons(e, new Null()));
  }

  private Object parseQuasiquote(PushbackReader reader) throws IOException
  {
    int c = reader.read();
    Object e = parse(c, reader);
    return Pair.cons(new Sym("quasiquote"), Pair.cons(e, new Null()));
  }

  private Object parseUnquote(PushbackReader reader) throws IOException
  {
    int lookahead = reader.read();
    if (lookahead == '@')
    {
      lookahead = reader.read();
      return Pair.cons(new Sym("unquote-splicing"), Pair.cons(parse(lookahead, reader), new Null()));
    }
    return Pair.cons(new Sym("unquote"), Pair.cons(parse(lookahead, reader), new Null()));
  }

  private String parseString(Reader reader) throws IOException
  {
    StringBuilder sb = new StringBuilder();
    int c;
    while ((c = reader.read()) != '"')
    {
      if (c == -1)
      {
        throw new StremeException("unmatched '\"'");
      }
      if (c == '\\')
      {
        c = reader.read();
      }
      sb.append((char) c);
    }
    return sb.toString();
  }

  private Object parseNumber(int c, PushbackReader reader) throws IOException
  {
    StringBuilder sb = new StringBuilder();
    sb.append((char) c);
    boolean dot = false;
    boolean slash = false;
    while (!Character.isWhitespace(c = reader.read()) && c != ')' && c != -1)
    {
      sb.append((char) c);
      dot |= (c == '.');
      slash |= (c == '/');
    }
    if (c == ')')
    {
      reader.unread(c);
    }
    if (slash)
    {
      return BigRational.checkRatOrInt(new BigRational(sb.toString()));
    }
    if (dot)
    {
      return Double.valueOf(sb.toString());
    }
    else
    {
      return Integer.valueOf(sb.toString());
    }
  }

  private Object parseList(PushbackReader reader) throws IOException
  {
    int lookahead = skipWhiteSpace(reader);
    if (lookahead == ')')
    {
      return new Null();
    }
    else
    {
      List<Object> expressions = new ArrayList<Object>();
      do
      {
        expressions.add(parse(lookahead, reader));
        lookahead = skipWhiteSpace(reader);
        if (lookahead == -1)
        {
          throw new StremeException("unmatched '('");
        }
        if (lookahead == '.')
        {
          lookahead = skipWhiteSpace(reader);
          Object l = parse(lookahead, reader);
          reader.read(); // ')'
          for (int i = expressions.size() - 1; i > -1; i--)
          {
            l = Pair.cons(expressions.get(i), l);
          }
          return l;
        }
      }
      while (lookahead != ')');
      Lst l = new Null();
      for (int i = expressions.size() - 1; i > -1; i--)
      {
        l = Pair.cons(expressions.get(i), l);
      }
      return l;
    }
  }

  private Object[] parseVector(PushbackReader reader) throws IOException
  {
    int lookahead = skipWhiteSpace(reader);
    if (lookahead == ')')
    {
      return EMPTY_VECTOR;
    }
    else
    {
      List<Object> expressions = new ArrayList<Object>();
      do
      {
        expressions.add(parse(lookahead, reader));
        lookahead = skipWhiteSpace(reader);
      }
      while (lookahead != ')');
      return expressions.toArray();
    }
  }

  private int skipWhiteSpace(Reader reader) throws IOException
  {
    int c;
    while (true)
    {
      while (Character.isWhitespace(c = reader.read()));
      if (c == ';')
      {
        while ((c = reader.read()) != '\n'); 
      }
      else
      {
        break;
      }
    }
    return c;
  }

  public static void main(String[] args)
  {
//    String source = "(define m -1)";
    String source = "(- a b)";
    Parser2 parser = new Parser2();
    Object p2 = parser.parse(source);
    System.out.println("new: " + p2);
  }
}
