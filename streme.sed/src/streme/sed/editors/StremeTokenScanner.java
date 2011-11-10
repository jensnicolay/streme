package streme.sed.editors;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.text.TextAttribute;
import org.eclipse.jface.text.rules.IRule;
import org.eclipse.jface.text.rules.IToken;
import org.eclipse.jface.text.rules.IWordDetector;
import org.eclipse.jface.text.rules.RuleBasedScanner;
import org.eclipse.jface.text.rules.SingleLineRule;
import org.eclipse.jface.text.rules.Token;
import org.eclipse.jface.text.rules.WhitespaceRule;
import org.eclipse.swt.SWT;

public class StremeTokenScanner extends RuleBasedScanner
{

  public StremeTokenScanner(ColorManager manager)
  {
    IToken string = new Token(new TextAttribute(manager.getColor(IXMLColorConstants.STRING)));
    IToken identifier = new Token(new TextAttribute(manager.getColor(IXMLColorConstants.DEFAULT)));
    IToken specialForm = new Token(new TextAttribute(manager.getColor(IXMLColorConstants.SPECIAL_FORM), null, SWT.BOLD));
    IToken primitive = new Token(new TextAttribute(manager.getColor(IXMLColorConstants.PRIMITIVE), null, SWT.BOLD));
    
    CombinedWordRule.WordMatcher matcher = new CombinedWordRule.WordMatcher();
    matcher.addWord("define", specialForm);
    matcher.addWord("lambda", specialForm);
    matcher.addWord("if", specialForm);
    matcher.addWord("begin", specialForm);
    matcher.addWord("set!", specialForm);
    matcher.addWord("let", specialForm);
    matcher.addWord("let*", specialForm);
    matcher.addWord("letrec", specialForm);
    matcher.addWord("and", specialForm);
    matcher.addWord("or", specialForm);
    matcher.addWord("cond", specialForm);
    matcher.addWord("else", specialForm);
    matcher.addWord("not", primitive);
    matcher.addWord("list", primitive);
    matcher.addWord("cons", primitive);
    matcher.addWord("+", primitive);
    matcher.addWord("-", primitive);
    matcher.addWord("*", primitive);
    matcher.addWord("/", primitive);
       CombinedWordRule specialFormsRule = new CombinedWordRule(new IWordDetector()
    {
      
      @Override
      public boolean isWordStart(char c)
      {
        return Character.isLowerCase(c);
      }
      
      @Override
      public boolean isWordPart(char c)
      {
        return Character.isLowerCase(c) || c == '-' || c == '!' || c == '*' || c == '/';
      }
    }, matcher, identifier);
    
    List<IRule> rules = new ArrayList<IRule>();
    rules.add(new SingleLineRule("\"", "\"", string, '\\'));
    rules.add(specialFormsRule);
    rules.add(new WhitespaceRule(new XMLWhitespaceDetector()));
    setRules(rules.toArray(new IRule[rules.size()]));
  }
}
