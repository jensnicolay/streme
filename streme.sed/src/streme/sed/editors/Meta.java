package streme.sed.editors;

import java.util.Map;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.text.IDocumentExtension3;
import org.eclipse.jface.text.Position;
import org.eclipse.jface.text.source.Annotation;
import org.eclipse.jface.text.source.DefaultCharacterPairMatcher;
import org.eclipse.jface.text.source.IAnnotationModel;
import org.eclipse.jface.text.source.IAnnotationModelExtension;
import org.eclipse.jface.text.source.ICharacterPairMatcher;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.texteditor.AbstractDecoratedTextEditor;
import org.eclipse.ui.texteditor.SourceViewerDecorationSupport;

import streme.lang.ast.Node;
import streme.sed.StremeAstAnalysisProvider2;

public class Meta extends AbstractDecoratedTextEditor
{
	public final static String EDITOR_MATCHING_BRACKETS = "matchingBrackets";
	public final static String EDITOR_MATCHING_BRACKETS_COLOR = "matchingBracketsColor";
	private static final char[] BRACKETS = { '(', ')', '[', ']' };
	private Annotation[] currentAnnotations;
	private ColorManager colorManager = new ColorManager();
	ICharacterPairMatcher bracketMatcher = new DefaultCharacterPairMatcher(BRACKETS,
			IDocumentExtension3.DEFAULT_PARTITIONING);

	public Meta()
	{
		super();
		setSourceViewerConfiguration(new MetaConfiguration(colorManager));
		setDocumentProvider(new StremeDocumentProvider());
	}

	public void doSave(IProgressMonitor progressMonitor)
	{
		super.doSave(progressMonitor);
		StremeAstAnalysisProvider2.getProvider().reset();
	}

	@Override
	protected void configureSourceViewerDecorationSupport(SourceViewerDecorationSupport support)
	{
		super.configureSourceViewerDecorationSupport(support);

		support.setCharacterPairMatcher(bracketMatcher);
		support.setMatchingCharacterPainterPreferenceKeys(EDITOR_MATCHING_BRACKETS, EDITOR_MATCHING_BRACKETS_COLOR);

		// Enable bracket highlighting in the preference store
		IPreferenceStore store = getPreferenceStore();
		store.setDefault(EDITOR_MATCHING_BRACKETS, true);
		store.setDefault(EDITOR_MATCHING_BRACKETS_COLOR, "128,128,128");
	}

	@Override
	public void createPartControl(Composite parent)
	{
		super.createPartControl(parent);
		StremeAstAnalysisProvider2.getProvider();

	}
	
	public static Annotation addAnnotation(Map<Annotation, Position> newAnnotations, Node node, String description, String type, boolean persistent)
	{
		Annotation annotation = new Annotation(type, persistent, description);
		newAnnotations.put(annotation, createPosition(node));
		return annotation;
	}
	
	public synchronized void replaceAnnotations(Map<Annotation, Position> newAnnotations)
	{
		System.out.println(this.getPartName() + " replacing " + currentAnnotations + " with " + newAnnotations);
		IAnnotationModel annotationModel = getDocumentProvider().getAnnotationModel(getEditorInput());
		((IAnnotationModelExtension) annotationModel).replaceAnnotations(currentAnnotations, newAnnotations);
		currentAnnotations = newAnnotations.keySet().toArray(new Annotation[newAnnotations.keySet().size()]);
	}

	public void dispose()
	{
		colorManager.dispose();
		bracketMatcher.dispose();
		// StremeAstAnalysisProvider.getProvider().reset();
		super.dispose();
	}

	public static Position createPosition(Node node)
	{
		Position position = new Position((Integer) node.getProperty("pos") - 7, (Integer) node.getProperty("length"));
		return position;
	}

}
