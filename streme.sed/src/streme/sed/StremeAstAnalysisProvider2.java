package streme.sed;

import streme.lang.analysis.StremeAstAnalysis;

public class StremeAstAnalysisProvider2 implements IEditorChangeListener
{
	private static StremeAstAnalysisProvider2 provider = new StremeAstAnalysisProvider2();
	private AstAnalysisCalculator calculator = new AstAnalysisCalculator();
	
	public StremeAstAnalysisProvider2()
	{
		super();
		EditorChangeProvider.getProvider().addListener(this);
	}
	
	public static StremeAstAnalysisProvider2 getProvider()
	{
		return provider;
	}
	
	public StremeAstAnalysis getAstAnalysis()
	{
		return calculator.get();
	}

	public void latestStremeEditorChanged(EditorChangeProvider provider)
	{
		calculator.calculateFor(provider.getActiveEditor(), false);
	}

	public void latestMetaEditorChanged(EditorChangeProvider provider)
	{
	}

	public void activeEditorChanged(EditorChangeProvider provider)
	{
	}

	public void reset()
	{
		calculator.calculateFor(EditorChangeProvider.getProvider().getActiveEditor(), true);
	}

}
