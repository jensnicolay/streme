package streme.lang.ast;

public interface AstAnalyzer<T>
{
  T analyze(Node node);
}
