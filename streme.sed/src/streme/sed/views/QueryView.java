package streme.sed.views;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;

import org.eclipse.compare.CompareConfiguration;
import org.eclipse.compare.CompareUI;
import org.eclipse.core.resources.IStorage;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.Position;
import org.eclipse.jface.text.source.Annotation;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.ListViewer;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerComparator;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.IPersistableElement;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.IStorageEditorInput;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchActionConstants;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.DrillDownAdapter;
import org.eclipse.ui.part.ViewPart;
import org.eclipse.ui.texteditor.ITextEditor;

import streme.lang.analysis.ParentAnalysis;
import streme.lang.analysis.ParentAnalyzer;
import streme.lang.ast.Begin;
import streme.lang.ast.Literal;
import streme.lang.ast.Node;
import streme.lang.ast.impl.Printer;
import streme.lang.ast.impl.SpPrinter;
import streme.lang.ast.impl.StremeSpDataCompiler2;
import streme.lang.data.Data;
import streme.lang.data.DataTemplate;
import streme.lang.data.Lst;
import streme.lang.data.SpParser2;
import streme.lang.data.Sym;
import streme.lang.eval.nd.NdStremePrimitives;
import streme.sed.Activator;
import streme.sed.EditorChangeProvider;
import streme.sed.EditorInput;
import streme.sed.IEditorChangeListener;
import streme.sed.editors.Sed;
import streme.lang.eval.spatt.Spatt;
import streme.lang.eval.tanfe.StremeContext;

public class QueryView extends ViewPart
{
	private static final AtomicInteger COUNTER = new AtomicInteger();

	/**
	 * The ID of the view as specified by the extension.
	 */
	public static final String ID = "streme.sed.views.QueryView";

	static
	{
		NdStremePrimitives.setClassLoader(QueryView.class.getClassLoader());
	}

	private TreeViewer treeViewer;
	private DrillDownAdapter drillDownAdapter;
	private Action action1;
	private Action action2;
	private Action doubleClickAction;
	private ListViewer metaEditorsViewer;
	private Object metaResult;

	private class OpenNewEditor extends Action
	{
		public void run()
		{
			ISelection selection = treeViewer.getSelection();
			Object obj = ((IStructuredSelection) selection).getFirstElement();
			if (obj instanceof TreeObject)
			{
				final Object o = ((TreeObject) obj).getObject();
				final IStorage storage = new IStorage()
				{

					public boolean equals(Object obj)
					{
						return o.equals(obj);
					}

					public int hashCode()
					{
						return o.hashCode();
					}

					@Override
					public Object getAdapter(Class adapter)
					{
						return null;
					}

					@Override
					public InputStream getContents() throws CoreException
					{
						String text;
						if (o instanceof Node)
						{
							Node node = (Node) o;
							if (isRewrittenRoot(node))
							{
								Begin begin = (Begin) node;
								StringBuilder texts = new StringBuilder();
								for (Node exp : begin.getExps())
								{
									texts.append(Printer.print(exp));
									texts.append("\n");
								}
								text = texts.toString();
							}
							else
							{
								text = Printer.print(node);
							}
						}
						else
						{
							text = String.valueOf(o);
						}
						return new ByteArrayInputStream(text.getBytes());
					}

					@Override
					public IPath getFullPath()
					{
						return null;
					}

					@Override
					public String getName()
					{
						return "contentsy";
					}

					@Override
					public boolean isReadOnly()
					{
						return false;
					}
				};
				IStorageEditorInput editorInput = new IStorageEditorInput()
				{

					@Override
					public Object getAdapter(Class adapter)
					{
						return null;
					}

					@Override
					public String getToolTipText()
					{
						return null;
					}

					@Override
					public IPersistableElement getPersistable()
					{
						return null;
					}

					@Override
					public String getName()
					{
						return "blah";
					}

					@Override
					public ImageDescriptor getImageDescriptor()
					{
						return null;
					}

					@Override
					public boolean exists()
					{
						return false;
					}

					@Override
					public IStorage getStorage() throws CoreException
					{
						return storage;
					}

					public boolean equals(Object object)
					{
						try
						{
							return object instanceof IStorageEditorInput
									&& getStorage().equals(((IStorageEditorInput) object).getStorage());
						}
						catch (CoreException ce)
						{
							return false;
						}
					}

					public int hashCode()
					{
						try
						{
							return getStorage().hashCode();
						}
						catch (CoreException ce)
						{
							ce.printStackTrace();
							return -1;
						}
					}
				};
				try
				{
					PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage()
							.openEditor(editorInput, "streme.sed.editors.Sed");
				}
				catch (PartInitException e)
				{
					e.printStackTrace();
				}
			}
		}
	}

	private class TreeObject implements IAdaptable
	{
		private Object object;
		private TreeParent parent;
		private ITextEditor editor;

		public TreeObject(Object object, ITextEditor editor)
		{
			super();
			this.object = object;
			this.editor = editor;
		}

		public Object getObject()
		{
			return object;
		}

		public ITextEditor getEditor()
		{
			return editor;
		}

		public void setParent(TreeParent parent)
		{
			this.parent = parent;
		}

		public TreeParent getParent()
		{
			return parent;
		}

		public String toString()
		{
			return String.valueOf(object);
		}

		public Object getAdapter(Class key)
		{
			return null;
		}
	}

	class TreeParent extends TreeObject
	{
		private ArrayList children;

		public TreeParent(String name)
		{
			super(name, null);
			children = new ArrayList();
		}

		public void addChild(TreeObject child)
		{
			children.add(child);
			child.setParent(this);
		}

		public void removeChild(TreeObject child)
		{
			children.remove(child);
			child.setParent(null);
		}

		public TreeObject[] getChildren()
		{
			return (TreeObject[]) children.toArray(new TreeObject[children.size()]);
		}

		public boolean hasChildren()
		{
			return children.size() > 0;
		}
	}

	class ViewContentProvider implements IStructuredContentProvider, ITreeContentProvider
	{
		private TreeParent invisibleRoot;

		public void inputChanged(Viewer v, Object oldInput, Object newInput)
		{
			invisibleRoot = new TreeParent("result");
			if (newInput == null)
			{
				return;
			}
			QueryModel queryModel = (QueryModel) newInput;
			Object metaResult = queryModel.getMetaResult();
			ITextEditor editor = queryModel.getEditor();
			if (metaResult instanceof Iterable)
			{
				for (Object o : (Iterable) metaResult)
				{
					// if (o instanceof Node)
					{
						invisibleRoot.addChild(new TreeObject(o, editor));
					}
				}
			}
			else
			{
              invisibleRoot.addChild(new TreeObject(metaResult, editor));			  
			}
			v.refresh();
		}

		public void dispose()
		{
		}

		public Object[] getElements(Object parent)
		{
			if (parent instanceof QueryModel)
			{
				return getChildren(invisibleRoot);
			}
			return getChildren(parent);
		}

		public Object getParent(Object child)
		{
			if (child instanceof TreeObject)
			{
				return ((TreeObject) child).getParent();
			}
			return null;
		}

		public Object[] getChildren(Object parent)
		{
			if (parent instanceof TreeParent)
			{
				return ((TreeParent) parent).getChildren();
			}
			return new Object[0];
		}

		public boolean hasChildren(Object parent)
		{
			if (parent instanceof TreeParent)
				return ((TreeParent) parent).hasChildren();
			return false;
		}
	}

	class ViewLabelProvider extends LabelProvider
	{

		private Image value = Activator.getImageDescriptor("icons/value.gif").createImage();
		private Image nodeFromSource = Activator.getImageDescriptor("icons/nodeFromSource.gif").createImage();
		private Image newNode = Activator.getImageDescriptor("icons/newNode.gif").createImage();
		private Image refactoringProposal = Activator.getImageDescriptor("icons/refactoringProposal.gif").createImage();

		public String getText(Object obj)
		{
			String string = obj.toString();
			if (obj instanceof TreeObject)
			{
				Object o = ((TreeObject) obj).getObject();
				if (o instanceof Node)
				{
					if (isRewrittenRoot((Node) o))
					{
						if (o instanceof Begin)
						{
							return Data.toStrings(((Begin) o).getExps());
						}
						return string;
					}
				}
			}
			return string;
		}

		public Image getImage(Object obj)
		{
			if (obj instanceof TreeParent)
			{
				return PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJ_FOLDER);
			}
			Object o = ((TreeObject) obj).getObject();
			if (o instanceof Node)
			{
				Node node = (Node) o;
				if (isRewrittenRoot(node))
				{
					return refactoringProposal;
				}
				boolean fromSource = isNodeFromSource(node);
				return fromSource ? nodeFromSource : newNode;
			}
			return value;
		}

		@Override
		public void dispose()
		{
			super.dispose();
			value.dispose();
			value = null;
			nodeFromSource.dispose();
			nodeFromSource = null;
			newNode.dispose();
			newNode = null;
			refactoringProposal.dispose();
			refactoringProposal = null;
		}
	}

//	class NameSorter extends ViewerSorter
//	{
//	}

	/**
	 * The constructor.
	 */
	public QueryView()
	{
		super();
	}

	/**
	 * This is a callback that will allow us to create the viewer and initialize
	 * it.
	 */
	public void createPartControl(Composite parent)
	{

		final SashForm sash = new SashForm(parent, SWT.HORIZONTAL);
		Rectangle clientArea = parent.getClientArea();
		sash.setBounds(180, clientArea.y, 32, clientArea.height);
		sash.addListener(SWT.Selection, new Listener()
		{
			public void handleEvent(Event e)
			{
				sash.setBounds(e.x, e.y, e.width, e.height);
			}
		});

		treeViewer = new TreeViewer(sash, SWT.MULTI | SWT.H_SCROLL | SWT.V_SCROLL);
		drillDownAdapter = new DrillDownAdapter(treeViewer);
		treeViewer.setContentProvider(new ViewContentProvider());
		treeViewer.setLabelProvider(new ViewLabelProvider());
		// treeViewer.setSorter(new NameSorter());
		// treeViewer.setInput(getViewSite());

		metaEditorsViewer = new ListViewer(sash);
		metaEditorsViewer.setContentProvider(new IStructuredContentProvider()
		{

			@Override
			public void inputChanged(Viewer viewer, Object oldInput, Object newInput)
			{
			}

			@Override
			public void dispose()
			{
			}

			@Override
			public Object[] getElements(Object inputElement)
			{
				IWorkbench workbench = (IWorkbench) inputElement;
				IEditorReference[] editorReferences = workbench.getActiveWorkbenchWindow().getActivePage()
						.getEditorReferences();
				List<IEditorReference> refs = new ArrayList<IEditorReference>();
				for (IEditorReference ref : editorReferences)
				{
					if (EditorChangeProvider.isMetaEditor(ref))
					{
						refs.add(ref);
					}
				}
				return refs.toArray();
			}
		});
		metaEditorsViewer.setInput(PlatformUI.getWorkbench());
		metaEditorsViewer.setLabelProvider(new ILabelProvider()
		{

			@Override
			public void addListener(ILabelProviderListener listener)
			{
			}

			@Override
			public void dispose()
			{
			}

			@Override
			public boolean isLabelProperty(Object element, String property)
			{
				return false;
			}

			@Override
			public void removeListener(ILabelProviderListener listener)
			{
			}

			@Override
			public Image getImage(Object element)
			{
				return null;
			}

			@Override
			public String getText(Object element)
			{
				return ((IEditorReference) element).getPartName();
			}

		});
		metaEditorsViewer.setComparator(new ViewerComparator());

		// Create the help context id for the viewer's control
		PlatformUI.getWorkbench().getHelpSystem().setHelp(treeViewer.getControl(), "streme.sed.viewer");
		makeActions();
		hookContextMenu();
		hookDoubleClickAction();
		contributeToActionBars();

		EditorChangeProvider.getProvider().addListener(new IEditorChangeListener()
		{

			public void latestStremeEditorChanged(EditorChangeProvider provider)
			{
				action1.setToolTipText("Run selected metaprogram on " + provider.getLatestStremeEditor().getTitle());
			}

			public void latestMetaEditorChanged(EditorChangeProvider provider)
			{
			}

			public void activeEditorChanged(EditorChangeProvider provider)
			{
				metaEditorsViewer.refresh();
			}
		});
	}

	private void hookContextMenu()
	{
		MenuManager menuMgr = new MenuManager("#PopupMenu");
		menuMgr.setRemoveAllWhenShown(true);
		menuMgr.addMenuListener(new IMenuListener()
		{
			public void menuAboutToShow(IMenuManager manager)
			{
				QueryView.this.fillContextMenu(manager);
			}
		});
		Menu menu = menuMgr.createContextMenu(treeViewer.getControl());
		treeViewer.getControl().setMenu(menu);
		getSite().registerContextMenu(menuMgr, treeViewer);
	}

	private void contributeToActionBars()
	{
		IActionBars bars = getViewSite().getActionBars();
		fillLocalPullDown(bars.getMenuManager());
		fillLocalToolBar(bars.getToolBarManager());
	}

	private void fillLocalPullDown(IMenuManager manager)
	{
		manager.add(action1);
		manager.add(new Separator());
		manager.add(action2);
	}

	private void fillContextMenu(IMenuManager manager)
	{
		manager.add(action1);
		manager.add(action2);
		manager.add(new Separator());
		drillDownAdapter.addNavigationActions(manager);
		// Other plug-ins can contribute there actions here
		manager.add(new Separator(IWorkbenchActionConstants.MB_ADDITIONS));
	}

	private void fillLocalToolBar(IToolBarManager manager)
	{
		manager.add(action1);
		manager.add(action2);
		manager.add(new Separator());
		drillDownAdapter.addNavigationActions(manager);
	}

	private void makeActions()
	{
		action1 = new Action()
		{
			public void run()
			{
				IEditorReference metaEditorRef = (IEditorReference) ((IStructuredSelection) metaEditorsViewer.getSelection())
						.getFirstElement();
				ITextEditor metaEditor = (ITextEditor) metaEditorRef.getEditor(true);
				IDocument metaDocument = metaEditor.getDocumentProvider().getDocument(metaEditor.getEditorInput());
				String metaSource = metaDocument.get();

				runMeta(metaSource);
			}

		};
		action1.setText("Run meta");
		action1.setImageDescriptor(Activator.getImageDescriptor("icons/run_exc.gif"));

		action2 = new Action()
		{
			public void run()
			{
				try
				{
					showMessage(String.valueOf(Class.forName("org.eclipse.ui.PlatformUI")));
				}
				catch (ClassNotFoundException e)
				{
					e.printStackTrace();
				}
			}
		};
		action2.setText("Action 2");
		action2.setToolTipText("Action 2 tooltip");
		action2.setImageDescriptor(PlatformUI.getWorkbench().getSharedImages()
				.getImageDescriptor(ISharedImages.IMG_OBJS_INFO_TSK));
		doubleClickAction = new Action()
		{
			@Override
			public void run()
			{
				ISelection selection = treeViewer.getSelection();
				Object obj = ((IStructuredSelection) selection).getFirstElement();
				TreeObject o = (TreeObject) obj;
				CompareConfiguration compareConfiguration = new CompareConfiguration();
				IDocument sourceDocument = o.getEditor().getDocumentProvider().getDocument(o.getEditor().getEditorInput());
				String source = sourceDocument.get();
				Node node = (Node) o.getObject();
				String proposed = SpPrinter.print(node);
				CompareUI.openCompareDialog(new EditorInput(compareConfiguration, source, proposed));
			}

		};
	}

	private void markQueryResults(Map<Annotation, Position> annotations, Object o, Node root)
	{
		if (o instanceof Node && o != root)
		{
			Node node = (Node) o;
			if (isNodeFromSource(node))
			{
				Sed.addAnnotation(annotations, (Node) o, "match", "streme.sed.result", true);
			}
		}
		else if (o instanceof Iterable)
		{
			for (Object oo : (Iterable) o)
			{
				markQueryResults(annotations, oo, root);
			}
		}
	}

	public boolean isNodeFromSource(Node node)
	{
		return Boolean.TRUE.equals(node.getProperty("fromSource"));
	}

	private void hookDoubleClickAction()
	{
		treeViewer.addDoubleClickListener(new IDoubleClickListener()
		{
			public void doubleClick(DoubleClickEvent event)
			{
				doubleClickAction.run();
			}
		});

		treeViewer.addPostSelectionChangedListener(new ISelectionChangedListener()
		{

			@Override
			public void selectionChanged(SelectionChangedEvent event)
			{
				IStructuredSelection selection = (IStructuredSelection) event.getSelection();
				// System.out.println("provider " + event.getSelectionProvider() +
				// " source " + event.getSource());

				if (selection.isEmpty())
				{
					return;
				}

				HashMap<Annotation, Position> newAnnotations = new HashMap<Annotation, Position>();
				Iterator<TreeObject> iter = selection.iterator();
				while (iter.hasNext())
				{
					Object object = iter.next().getObject();
					if (object instanceof Node)
					{
						Node node = (Node) object;
						if (isNodeFromSource(node))
						{
							Sed.addAnnotation(newAnnotations, (Node) object, "match", "streme.sed.result", true);
						}
					}
				}
				Sed sourceEditor = (Sed) EditorChangeProvider.getProvider().getLatestStremeEditor();
				sourceEditor.replaceAnnotations(newAnnotations);
			}
		});
	}

	private void showMessage(String message)
	{
		MessageDialog.openInformation(treeViewer.getControl().getShell(), "Query View", message);
	}

	/**
	 * Passing the focus request to the viewer's control.
	 */
	public void setFocus()
	{
		treeViewer.getControl().setFocus();
	}

	@Override
	public void dispose()
	{
		super.dispose();
	}

	public void runMeta(String metaSource)
	{
		Sed sourceEditor = (Sed) EditorChangeProvider.getProvider().getLatestStremeEditor();
		IDocument sourceDocument = sourceEditor.getDocumentProvider().getDocument(sourceEditor.getEditorInput());
		String source = sourceDocument.get();
		SpParser2 parser = new SpParser2(source);
		List<Object> datas = parser.all();
		StremeSpDataCompiler2 compiler = new StremeSpDataCompiler2(parser.getSps());
		Node ast = compiler.compileBody(Lst.valueOf(datas));
		ast.setProperty("root", true);
		Spatt spatt = new Spatt();
		spatt.globalEnv().add(new Sym("*ast*"), ast);

//		IStructuredSelection restrictionSelection = (IStructuredSelection) treeViewer.getSelection();
//		if (restrictionSelection.isEmpty())
		
		metaResult = spatt.readEval(metaSource, spatt.globalEnv());
		tagNodes(metaResult, spatt);
		System.out.println(metaResult.getClass() + ": " + metaResult);
		QueryModel queryModel = new QueryModel(metaResult, sourceEditor);
		treeViewer.setInput(queryModel);
		Map<Annotation, Position> newAnnotations = new HashMap<Annotation, Position>();
		markQueryResults(newAnnotations, metaResult, ast);
		sourceEditor.replaceAnnotations(newAnnotations);
	}

	private void tagNodes(Object result, Spatt context)
	{
		if (result instanceof Node)
		{
			Node node = (Node) result;
			boolean fromSource = !Boolean.FALSE.equals(context.readEval("(memq ~0 (nodes *ast*))", node));
			if (fromSource)
			{
				node.setProperty("fromSource", true);
			}
		}
		else if (result instanceof Iterable)
		{
			for (Object o : (Iterable) result)
			{
				tagNodes(o, context);
			}
		}
	}

	public boolean isRewrittenRoot(Node node)
	{
		return Boolean.TRUE.equals(node.getProperty("root"));
	}

}