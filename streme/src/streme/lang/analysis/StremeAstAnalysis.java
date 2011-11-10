package streme.lang.analysis;

import streme.lang.ast.Node;

public class StremeAstAnalysis
{
	private Node ast;
	private ParentAnalysis parentAnalysis;
	private VarPointerAnalysis varPointerAnalysis;
	private IpdAnalysis ipdAnalysis;
	
	public StremeAstAnalysis(Node ast, ParentAnalysis parentAnalysis,
			VarPointerAnalysis varPointerAnalysis, IpdAnalysis ipdAnalysis)
	{
		super();
		this.ast = ast;
		this.parentAnalysis = parentAnalysis;
		this.varPointerAnalysis = varPointerAnalysis;
		this.ipdAnalysis = ipdAnalysis;
	}
	public Node getAst()
	{
		return ast;
	}
	public ParentAnalysis getParentAnalysis()
	{
		return parentAnalysis;
	}
	public VarPointerAnalysis getVarPointerAnalysis()
	{
		return varPointerAnalysis;
	}
	public IpdAnalysis getIpdAnalysis()
	{
		return ipdAnalysis;
	}
	
	
}
