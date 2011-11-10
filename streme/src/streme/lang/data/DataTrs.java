package streme.lang.data;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;


public class DataTrs
{
  private DataUnifier unifier = new DataUnifier();
  private List<DataRule> rules;

  public DataTrs()
  {
    super();
    rules = new ArrayList<DataRule>();
  }

  public void addRule(DataRule rule)
  {
    rules.add(rule);
  }

  public Object rewrite(Object object)
  {
    Object current;
    Object rewritten = object;
    do
    {
      current = rewritten;
      rewritten = rewrite0(current);
    }
    while (rewritten != null);
    return current;
  }

  private Object rewrite0(Object object)
  {
    for (DataRule rule : rules)
    {
      Object lhs = rule.lhs();
      Map<Sym, Object> subs = unifier.unify(object, lhs);
      if (subs != null)
      {
        Object rewritten = matched(rule, subs);
        if (rewritten != null)
        {
          return rewritten;
        }
      }
    }
    if (object instanceof Pair)
    {
      Pair p = (Pair) object;
      Object head = p.car();
      Object tail = p.cdr();
      Object r = rewrite0(head);
      if (r != null)
      {
        return new Pair(r, tail);
      }
      Object rr = rewrite0(tail);
      if (rr != null)
      {
        return new Pair(head, rr);
      }
    }
    return null;
  }

  protected Object matched(DataRule rule, Map<Sym, Object> subs)
  {
    return unifier.apply(subs, rule.rhs(), false);
  }
  
  public DataUnifier getUnifier()
  {
    return unifier;
  }
  
  public static void main(String[] args)
  {
    Parser2 parser = new Parser2();
    DataRule r1 = new DataRule("r1", parser.parse("(or #t ?x)"), parser.parse("#t"));
    DataTrs trs = new DataTrs();
    trs.addRule(r1);
    System.out.println(trs.rewrite(parser.parse("(or #t 124)")));
  }
}
