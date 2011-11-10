package streme.sed.views;

import org.eclipse.ui.texteditor.ITextEditor;

public class QueryModel
{

	private Object metaResult;
	private ITextEditor editor;

	public QueryModel(Object metaResult, ITextEditor editor)
	{
		super();
		this.metaResult = metaResult;
		this.editor = editor;
	}

	public Object getMetaResult()
	{
		return metaResult;
	}

	public ITextEditor getEditor()
	{
		return editor;
	}

}
