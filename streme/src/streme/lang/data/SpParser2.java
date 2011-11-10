package streme.lang.data;

import java.io.IOException;
import java.io.PushbackReader;
import java.io.Reader;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.IdentityHashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import streme.lang.StremeException;

/**
 * Implementation note: this parser makes new objects on purpose (for source position tracking reasons).
 */
public class SpParser2
{
  
  private static class ParserReader extends PushbackReader
  {
    private int pos = -1;
    private int line;
    private int linePos = -1;

    public ParserReader(Reader reader)
    {
      super(reader);
    }

    public int read() throws IOException
    {
      int r = super.read();
      if (r == '\n')
      {
        linePos = -1;
        line++;
      }
      else
      {
        linePos++;
      }
      pos++;
      return r;
    }

    public void unread(int r) throws IOException
    {
      if (r == '\n')
      {
        throw new UnsupportedOperationException();
      }
      super.unread(r);
      pos--;
      linePos--;
    }

    public int line()
    {
      return line;
    }

    public int linePos()
    {
      return linePos;
    }
    
    public int pos()
    {
      return pos;
    }
  }


  private ParserReader parserReader;
  private Map<Object, SpData> sps;
  private String prefix;
  private String suffix;
  
  
  public SpParser2(CharSequence chars)
  {
    this(new StringReader(chars.toString()));
  }

  public SpParser2(Reader reader)
  {
    super();
    parserReader = new ParserReader(reader);
    sps = new IdentityHashMap<Object, SpData>();
  }

  public Object next()
  {
    try
    {
      int c = skipWhiteSpace(parserReader, "");
      suffix = "";
      return parse(c, parserReader);
    }
    catch (Exception e)
    {
      throw new StremeException("error while parsing", e);
    }
  }
  
  public List<Object> all()
  {
    List<Object> datas = new ArrayList<Object>();
    Object data;
    while ((data = next()) != null)
    {
      datas.add(data);
    }
    return datas;
  }
  

  private Object parse(int c, ParserReader reader) throws IOException
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
          Boolean po = new Boolean(true);
          register(po, prefix, reader.pos() - 1, reader.line(), reader.linePos() - 1, 2, "");
          suffix = "";
          return po;
        }
        if (d == 'f')
        {
          Boolean po = new Boolean(false);
          register(po, prefix, reader.pos() - 1, reader.line(), reader.linePos() - 1, 2, "");
          suffix = "";
          return po;
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
        if (Character.isWhitespace(d) || d == ')' || d == -1)
        {
          reader.unread(d);
          Sym po = new Sym("-");
          register(po, prefix, reader.pos(), reader.line(), reader.linePos(), 1, "");
          suffix = "";
          return po;
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

  private Sym parseIdentifier(int c, ParserReader reader) throws IOException
  {
    int pos = reader.pos();
    int line = reader.line();
    int linePos = reader.linePos();
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
    int unread;
    if (c == ')')
    {
      reader.unread(c);
      unread = 1;
      suffix = "";
    }
    else
    {
      unread = 0;
      suffix = Character.toString((char) c);
    }
    String s = sb.toString();
    Sym po = new Sym(s);
    register(po, prefix, pos, line, linePos, reader.pos() - pos + unread, suffix);
    return po;
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

  private Object parseQuote(ParserReader reader) throws IOException
  {
    int pos = reader.pos();
    int line = reader.line();
    int linePos = reader.linePos();
    String prefix = this.prefix;
    int c = reader.read();
    Object e = parse(c, reader);
    Pair<Sym, Pair<Object, Null>> po = Pair.cons(new Sym("quote"), Pair.cons(e, new Null()));
    SpData spData = register(po, prefix, pos, line, linePos, sps.get(e).getLength() + 1, suffix);
    spData.setOriginal("'");
    return po;
  }

  private Object parseQuasiquote(ParserReader reader) throws IOException
  {
    int pos = reader.pos();
    int line = reader.line();
    int linePos = reader.linePos();
    int c = reader.read();
    Object e = parse(c, reader);
    Pair<Sym, Pair<Object, Null>> po = Pair.cons(new Sym("quasiquote"), Pair.cons(e, new Null()));
    SpData spData = register(po, prefix, pos, line, linePos, reader.pos() - pos, suffix);
    spData.setOriginal("`");
    return po;
  }

  private Object parseUnquote(ParserReader reader) throws IOException
  {
    int pos = reader.pos();
    int line = reader.line();
    int linePos = reader.linePos();
    int lookahead = reader.read();
    if (lookahead == '@')
    {
      lookahead = reader.read();
      Pair<Sym, Pair<Object, Null>> po = Pair.cons(new Sym("unquote-splicing"), Pair.cons(parse(lookahead, reader), new Null()));
      SpData spData = register(po, prefix, pos, line, linePos, reader.pos() - pos, suffix);
      spData.setOriginal(",@");
      return po;
    }
    Pair<Sym, Pair<Object, Null>> po = Pair.cons(new Sym("unquote"), Pair.cons(parse(lookahead, reader), new Null()));
    SpData spData = register(po, prefix, pos, line, linePos, reader.pos() - pos, suffix);
    spData.setOriginal(",");
    return po;
  }

  private String parseString(ParserReader reader) throws IOException
  {
    int pos = reader.pos();
    int line = reader.line();
    int linePos = reader.linePos();
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
    String po = sb.toString();
    register(po, prefix, pos, line, linePos, reader.pos() - pos + 1, "");
    suffix = "";
    return po;
  }

  private Object parseNumber(int c, ParserReader reader) throws IOException
  {
    int pos = reader.pos();
    int line = reader.line();
    int linePos = reader.linePos();
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
    int unread;
    if (c == ')')
    {
      reader.unread(c);
      unread = 1;
      suffix = "";
    }
    else
    {
      unread = 0;
      suffix = Character.toString((char) c);
    }
    Object po;
    if (slash)
    {
      po = BigRational.checkRatOrInt(new BigRational(sb.toString()));
    }
    if (dot)
    {
      po = new Double(sb.toString());
    }
    else
    {
      po = new Integer(sb.toString());
    }
    register(po, prefix, pos, line, linePos, reader.pos() - pos + unread, suffix);
    return po;
  }

  private SpData register(Object po, String prefix, int pos, int line, int linePos, int length, String suffix)
  {
    if (sps.containsKey(po))
    {
      throw new StremeException("cannot track source position for " + po);
    }
    SpData spData = new SpData(prefix, pos, line, linePos, length, suffix);
    sps.put(po, spData);
    return spData;
  }

  private Object parseList(ParserReader reader) throws IOException
  {
    int pos = reader.pos();
    int line = reader.line();
    int linePos = reader.linePos();
    String prefix = this.prefix;
    int lookahead = skipWhiteSpace(reader, "");
    if (lookahead == ')')
    {
      Null po = new Null();
      register(po, prefix, pos, line, linePos, 2, this.prefix);
      suffix = "";
      return po;
    }
    else
    {
      List<Object> expressions = new ArrayList<Object>();
      do
      {
        Object expression = parse(lookahead, reader);
        expressions.add(expression);
        lookahead = skipWhiteSpace(reader, suffix);
        if (lookahead == -1)
        {
          throw new StremeException("unmatched '('");
        }
        if (lookahead == '.')
        {
          lookahead = skipWhiteSpace(reader, " ");
          Object l = expression;
          reader.read(); // ')'
          for (int i = expressions.size() - 1; i > -1; i--)
          {
            l = Pair.cons(expressions.get(i), l);
          }
          register(l, prefix, pos, line, linePos, reader.pos() - pos + 1, this.prefix);
          suffix = "";
          return l;
        }
      }
      while (lookahead != ')');
      Lst l = new Null();
      for (int i = expressions.size() - 1; i > -1; i--)
      {
        l = Pair.cons(expressions.get(i), l);
      }
      register(l, prefix, pos, line, linePos, reader.pos() - pos + 1, this.prefix);
      suffix = "";
      return l;
    }
  }

  private Object parseVector(ParserReader reader) throws IOException
  {
    int pos = reader.pos() - 1;
    int line = reader.line();
    int linePos = reader.linePos() - 1;
    int lookahead = skipWhiteSpace(reader, "");
    Object po;
    if (lookahead == ')')
    {
      po = new Object[0];
    }
    else
    {
      List<Object> expressions = new ArrayList<Object>();
      do
      {
        expressions.add(parse(lookahead, reader));
        lookahead = skipWhiteSpace(reader, suffix);
      }
      while (lookahead != ')');
      po = expressions.toArray();
    }
    register(po, prefix, pos, line, linePos, reader.pos() - pos + 1, suffix);
    return po;
  }

  private int skipWhiteSpace(Reader reader, String prefix) throws IOException
  {
    StringBuilder sb = new StringBuilder(prefix);
    int c;
    while (true)
    {
      c = reader.read();
      while (Character.isWhitespace(c))
      {
        sb.append((char) c);
        c = reader.read();
      }
      if (c == ';')
      {
        c = reader.read();
        while (c != '\n')
        {
          sb.append((char) c);
          c = reader.read();          
        }
      }
      else
      {
        break;
      }
    }
    this.prefix = sb.toString();
    return c;
  }

  public Map<Object, SpData> getSps()
  {
    return sps;
  }
  
  public static void main(String[] args)
  {
    String source = "(define a 1) (define b 2)";
    SpParser2 spParser = new SpParser2(source);
    Object sp = spParser.next();
    System.out.println(sp);
    sp = spParser.next();
    System.out.println(sp);
    sp = spParser.next();
    System.out.println(sp);
    sp = spParser.next();
    System.out.println(sp);
    System.out.println(spParser.getSps());
  }
}
