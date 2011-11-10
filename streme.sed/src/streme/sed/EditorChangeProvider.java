package streme.sed;

import org.eclipse.core.commands.common.EventManager;
import org.eclipse.ui.IPartListener2;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchPartReference;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.texteditor.ITextEditor;

import streme.sed.editors.Sed;

public class EditorChangeProvider extends EventManager
{
	private static EditorChangeProvider provider = new EditorChangeProvider();

	public static boolean isStremeEditor(IWorkbenchPartReference ref)
	{
		if (ref == null)
			return false;
		IWorkbenchPart part = ref.getPart(false);
		return isStremeEditor(part);
	}

	public static boolean isStremeEditor(IWorkbenchPart part)
	{
		return part instanceof Sed;
	}
	
	public static boolean isMetaEditor(IWorkbenchPartReference ref)
	{
		if (ref == null)
			return false;
		IWorkbenchPart part = ref.getPart(false);
		return isMetaEditor(part);
	}

	public static boolean isMetaEditor(IWorkbenchPart part)
	{
		return part instanceof ITextEditor && ((ITextEditor) part).getTitle().endsWith(".meta");
	}

	
	private class ActivationListener implements IPartListener2
	{

		public void partActivated(IWorkbenchPartReference ref)
		{
			updateLatestEditors(ref);
		}

		public void partBroughtToTop(IWorkbenchPartReference ref)
		{
			updateLatestEditors(ref);
		}

		public void partClosed(IWorkbenchPartReference ref)
		{
		}

		public void partDeactivated(IWorkbenchPartReference ref)
		{
		}

		public void partHidden(IWorkbenchPartReference ref)
		{
		}

		public void partInputChanged(IWorkbenchPartReference ref)
		{
			updateLatestEditors(ref);
		}

		public void partOpened(IWorkbenchPartReference ref)
		{
			updateLatestEditors(ref);
		}

		public void partVisible(IWorkbenchPartReference ref)
		{
			updateLatestEditors(ref);
		}

		public void updateLatestEditors(IWorkbenchPartReference ref)
		{
			if (isStremeEditor(ref))
			{
				ITextEditor stremeEditor = (ITextEditor) ref.getPart(true);
				if (!stremeEditor.equals(activeEditor))
				{
					activeEditor = stremeEditor;
					if (!stremeEditor.equals(latestStremeEditor))
					{
						latestStremeEditor = stremeEditor;
						latestStremeEditorChanged();
					}
					activeEditorChanged();
				}
			}
			else if (isMetaEditor(ref))
			{
				ITextEditor metaEditor = (ITextEditor) ref.getPart(true);
				if (!metaEditor.equals(activeEditor))
				{
					activeEditor = metaEditor;
					if (!metaEditor.equals(latestMetaEditor))
					{
						latestMetaEditor = metaEditor;
						latestMetaEditorChanged();
					}
					activeEditorChanged();
				}
			}
		}
	}

	private ITextEditor activeEditor;
	private ITextEditor latestStremeEditor;
	private ITextEditor latestMetaEditor;
	private ActivationListener activationListener;
	
	public EditorChangeProvider()
	{
		super();
		activationListener = new ActivationListener();
		PlatformUI.getWorkbench().getActiveWorkbenchWindow().getPartService().addPartListener(activationListener);
	}
	
	public static EditorChangeProvider getProvider()
	{
		return provider;
	}
	
	public ITextEditor getActiveEditor()
	{
		return activeEditor;
	}
	
	public ITextEditor getLatestMetaEditor()
	{
		return latestMetaEditor;
	}
	
	public ITextEditor getLatestStremeEditor()
	{
		return latestStremeEditor;
	}

	private void latestStremeEditorChanged()
	{
		for (Object l : getListeners())
		{
			((IEditorChangeListener) l).latestStremeEditorChanged(this);
		}
	}
	
	private void latestMetaEditorChanged()
	{
		for (Object l : getListeners())
		{
			((IEditorChangeListener) l).latestMetaEditorChanged(this);
		}
	}
	
	private void activeEditorChanged()
	{
		for (Object l : getListeners())
		{
			((IEditorChangeListener) l).activeEditorChanged(this);
		}
	}
	
	public void addListener(IEditorChangeListener l)
	{
		addListenerObject(l);
	}

}
