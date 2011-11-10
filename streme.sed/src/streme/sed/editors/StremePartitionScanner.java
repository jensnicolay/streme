package streme.sed.editors;

import org.eclipse.jface.text.rules.IPredicateRule;
import org.eclipse.jface.text.rules.IToken;
import org.eclipse.jface.text.rules.MultiLineRule;
import org.eclipse.jface.text.rules.RuleBasedPartitionScanner;
import org.eclipse.jface.text.rules.Token;

public class StremePartitionScanner extends RuleBasedPartitionScanner
{

  public static final String BLOCK_COMMENT = "__block_comment";

  public StremePartitionScanner()
  {
    IToken blockComment = new Token(BLOCK_COMMENT);
    IPredicateRule[] rules = new IPredicateRule[1];
    rules[0] = new MultiLineRule("#|", "|#", blockComment);
    setPredicateRules(rules);
  }
}
