package streme.sed.editors;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.text.IDocumentExtension3;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.text.Position;
import org.eclipse.jface.text.source.Annotation;
import org.eclipse.jface.text.source.DefaultCharacterPairMatcher;
import org.eclipse.jface.text.source.IAnnotationModel;
import org.eclipse.jface.text.source.IAnnotationModelExtension;
import org.eclipse.jface.text.source.ICharacterPairMatcher;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.ISelectionListener;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.texteditor.AbstractDecoratedTextEditor;
import org.eclipse.ui.texteditor.IDocumentProvider;
import org.eclipse.ui.texteditor.ITextEditor;
import org.eclipse.ui.texteditor.SourceViewerDecorationSupport;

import streme.lang.analysis.StremeAstAnalysis;
import streme.lang.ast.Node;
import streme.lang.ast.Ref;
import streme.lang.ast.SetVar;
import streme.lang.ast.Var;
import streme.lang.ast.impl.SpNodeLocator;
import streme.lang.data.SpData;
import streme.sed.Activator;
import streme.sed.EditorChangeProvider;
import streme.sed.StremeAstAnalysisProvider2;

public class Sed extends AbstractDecoratedTextEditor
{
	public static class SedSelectionListener implements ISelectionListener
	{
		@Override
		public void selectionChanged(final IWorkbenchPart part, final ISelection selection)
		{
			new Job("annotate")
			{
				protected IStatus run(IProgressMonitor monitor)
				{
					try
					{
						if (EditorChangeProvider.isStremeEditor(part) && selection instanceof ITextSelection)
						{
							IDocumentProvider documentProvider = ((ITextEditor) part).getDocumentProvider();
							StremeAstAnalysis astAnalysis = StremeAstAnalysisProvider2.getProvider().getAstAnalysis();
							ITextSelection ts = (ITextSelection) selection;
							Node coveringNode = SpNodeLocator.locateCoveringNode(astAnalysis.getAst(), ts.getOffset(),
									ts.getLength());
							System.out.println("covering node: " + coveringNode);
							IAnnotationModel annotationModel = documentProvider.getAnnotationModel(((ITextEditor) part)
									.getEditorInput());
							Map<Annotation, Position> newAnnotations = new HashMap<Annotation, Position>();
							switch (coveringNode.type())
							{
							case VAR:
							{
								Node parent2 = astAnalysis.getParentAnalysis().getParent(coveringNode);
								Var var;
								if (parent2.type() == Node.Type.SETVAR)
								{
									var = markVarWritten((SetVar) parent2, astAnalysis, newAnnotations);
								}
								else
								{
									var = (Var) coveringNode;
									addAnnotation(newAnnotations, var, "var", "org.eclipse.jdt.ui.occurrences", false);
								}
								markReadRefs(var, astAnalysis, newAnnotations);
								markWriteRefs(var, astAnalysis, newAnnotations);
								break;
							}
							case REF:
							{
								Var varRead = astAnalysis.getVarPointerAnalysis().getVarRead((Ref) coveringNode);
								addAnnotation(newAnnotations, varRead, "varRead", "org.eclipse.jdt.ui.occurrences", false);
								markReadRefs(varRead, astAnalysis, newAnnotations);
								markWriteRefs(varRead, astAnalysis, newAnnotations);
								break;
							}
							case SETVAR:
							{
								SetVar setVar = (SetVar) coveringNode;
								Var varWritten = markVarWritten(setVar, astAnalysis, newAnnotations);
								markReadRefs(varWritten, astAnalysis, newAnnotations);
								markWriteRefs(varWritten, astAnalysis, newAnnotations);
								break;
							}
							}
							if (!newAnnotations.isEmpty())
							{
								((Sed) part).replaceAnnotations(newAnnotations);
							}
						}
						return new Status(IStatus.OK, Activator.PLUGIN_ID, "ok");
					}
					catch (Exception e)
					{
						return new Status(IStatus.ERROR, Activator.PLUGIN_ID, e.getMessage());
					}
				}
			}.run(new NullProgressMonitor());
		}

		public void markReadRefs(Var coveringNode, StremeAstAnalysis astAnalysis, Map<Annotation, Position> newAnnotations)
		{
			Set<Ref> readRefs = astAnalysis.getVarPointerAnalysis().getReadRefs(coveringNode);
			for (Ref readRef : readRefs)
			{
				addAnnotation(newAnnotations, readRef, "readRef", "org.eclipse.jdt.ui.occurrences", false);
			}
		}

		public void markWriteRefs(Var coveringNode, StremeAstAnalysis astAnalysis, Map<Annotation, Position> newAnnotations)
		{
			Set<SetVar> writeRefs = astAnalysis.getVarPointerAnalysis().getWriteRefs(coveringNode);
			for (SetVar writeRef : writeRefs)
			{
				addAnnotation(newAnnotations, writeRef, "writeRef", "org.eclipse.jdt.ui.occurrences.write", false);
			}
		}

		public Var markVarWritten(SetVar setVar, StremeAstAnalysis astAnalysis, Map<Annotation, Position> newAnnotations)
		{
			Var varWritten = astAnalysis.getVarPointerAnalysis().getVarWritten(setVar);
			addAnnotation(newAnnotations, varWritten, "varWritten", "org.eclipse.jdt.ui.occurrences", false);
			return varWritten;
		}
	}

	public final static String EDITOR_MATCHING_BRACKETS = "matchingBrackets";
	public final static String EDITOR_MATCHING_BRACKETS_COLOR = "matchingBracketsColor";
	private static final char[] BRACKETS = { '(', ')', '[', ']' };
	private Annotation[] currentAnnotations;
	private ColorManager colorManager = new ColorManager();
	ICharacterPairMatcher bracketMatcher = new DefaultCharacterPairMatcher(BRACKETS,
			IDocumentExtension3.DEFAULT_PARTITIONING);
	private static ISelectionListener selectionListener = new SedSelectionListener();

	public Sed()
	{
		super();
		setSourceViewerConfiguration(new StremeConfiguration(colorManager));
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
		getEditorSite().getPage().addPostSelectionListener(selectionListener);
		// addPropertyListener(new IPropertyListener()
		// {
		//
		// @Override
		// public void propertyChanged(Object source, int propId)
		// {
		// System.out.println(source + "  " + propId);
		// }
		// });
		// addPartPropertyListener(new IPropertyChangeListener()
		// {
		//
		// @Override
		// public void propertyChange(PropertyChangeEvent event)
		// {
		// System.out.println("*** event");
		// }
		// });

	}
	
	public static Annotation addAnnotation(Map<Annotation, Position> newAnnotations, Node node, String description, String type, boolean persistent)
	{
		Annotation annotation = new Annotation(type, persistent, description);
		newAnnotations.put(annotation, createPosition(node));
		return annotation;
	}
	
	public synchronized void replaceAnnotations(Map<Annotation, Position> newAnnotations)
	{
		//System.out.println(this.getPartName() + " replacing " + currentAnnotations + " with " + newAnnotations);
		IAnnotationModel annotationModel = getDocumentProvider().getAnnotationModel(getEditorInput());
		((IAnnotationModelExtension) annotationModel).replaceAnnotations(currentAnnotations, newAnnotations);
		currentAnnotations = newAnnotations.keySet().toArray(new Annotation[newAnnotations.keySet().size()]);
	}

	public void dispose()
	{
		getEditorSite().getPage().removePostSelectionListener(selectionListener);
		colorManager.dispose();
		bracketMatcher.dispose();
		// StremeAstAnalysisProvider.getProvider().reset();
		super.dispose();
	}

	public static Position createPosition(Node node)
	{
		SpData sp = (SpData) node.getProperty("sp");
		Position position = new Position(sp.getPos(), sp.getLength());
		return position;
	}

}
