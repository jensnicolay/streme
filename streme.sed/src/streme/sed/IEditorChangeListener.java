package streme.sed;

public interface IEditorChangeListener
{

	void latestStremeEditorChanged(EditorChangeProvider provider);
	void latestMetaEditorChanged(EditorChangeProvider provider);
	void activeEditorChanged(EditorChangeProvider provider);

}
