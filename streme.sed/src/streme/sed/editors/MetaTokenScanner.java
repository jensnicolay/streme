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

public class MetaTokenScanner extends RuleBasedScanner
{

  public MetaTokenScanner(ColorManager manager)
  {
    IToken string = new Token(new TextAttribute(manager.getColor(IXMLColorConstants.STRING)));
    IToken identifier = new Token(new TextAttribute(manager.getColor(IXMLColorConstants.DEFAULT)));
    IToken specialForm = new Token(new TextAttribute(manager.getColor(IXMLColorConstants.SPECIAL_FORM), null, SWT.BOLD));
    IToken primitive = new Token(new TextAttribute(manager.getColor(IXMLColorConstants.PRIMITIVE), null, SWT.BOLD));
    IToken queryPrimitive = new Token(new TextAttribute(manager.getColor(IXMLColorConstants.QUERY_PRIMITIVE), null, SWT.BOLD));
    IToken transformPrimitive = new Token(new TextAttribute(manager.getColor(IXMLColorConstants.TRANSFORM_PRIMITIVE), null, SWT.BOLD));

    CombinedWordRule.WordMatcher matcher = new CombinedWordRule.WordMatcher();
    matcher.addWord("and", specialForm);
    matcher.addWord("begin", specialForm);
    matcher.addWord("cond", specialForm);
    matcher.addWord("define", specialForm);
    matcher.addWord("if", specialForm);
    matcher.addWord("lambda", specialForm);
    matcher.addWord("let", specialForm);
    matcher.addWord("let*", specialForm);
    matcher.addWord("letrec", specialForm);
    matcher.addWord("or", specialForm);
    matcher.addWord("set!", specialForm);

    matcher.addWord("+", primitive);
    matcher.addWord("-", primitive);
    matcher.addWord("*", primitive);
    matcher.addWord("/", primitive);
    matcher.addWord(">", primitive);
    matcher.addWord(">=", primitive);
    matcher.addWord("apply", primitive);
    matcher.addWord("car", primitive);
    matcher.addWord("cdr", primitive);
    matcher.addWord("cons", primitive);
    matcher.addWord("eq?", primitive);
    matcher.addWord("length", primitive);
    matcher.addWord("list", primitive);
    matcher.addWord("not", primitive);
    matcher.addWord("null?", primitive);
    matcher.addWord("vector", primitive);
    
    matcher.addWord("application?", queryPrimitive);
    matcher.addWord("application-operands", queryPrimitive);
    matcher.addWord("application-operator", queryPrimitive);
    matcher.addWord("independent?", queryPrimitive);
    matcher.addWord("name-in-scope?", queryPrimitive);
    matcher.addWord("ref?", queryPrimitive);
    matcher.addWord("set?", queryPrimitive);
    matcher.addWord("set-var", queryPrimitive);
    matcher.addWord("set-value", queryPrimitive);
    matcher.addWord("var?", queryPrimitive);
    matcher.addWord("var-read", queryPrimitive);
    matcher.addWord("a-var-with-name", queryPrimitive);
    matcher.addWord("var-written", queryPrimitive);

    matcher.addWord("create-application", transformPrimitive);
    matcher.addWord("create-ref", transformPrimitive);
    matcher.addWord("create-set", transformPrimitive);
    matcher.addWord("create-var", transformPrimitive);
    matcher.addWord("rewrite", transformPrimitive);
    
    CombinedWordRule specialFormsRule = new CombinedWordRule(new IWordDetector()
    {
      
      @Override
      public boolean isWordStart(char c)
      {
        return Character.isLowerCase(c) || c == '-' || c == '?' || c == '!' || c == '*' || c == '/'|| c == '+'|| c == '+';
      }
      
      @Override
      public boolean isWordPart(char c)
      {
        return Character.isLowerCase(c) || c == '-' || c == '?' || c == '!' || c == '*' || c == '/'|| c == '+';
      }
    }, matcher, identifier);
    
    List<IRule> rules = new ArrayList<IRule>();
    rules.add(new SingleLineRule("\"", "\"", string, '\\'));
    rules.add(specialFormsRule);
    rules.add(new WhitespaceRule(new XMLWhitespaceDetector()));
    setRules(rules.toArray(new IRule[rules.size()]));
  }
}
