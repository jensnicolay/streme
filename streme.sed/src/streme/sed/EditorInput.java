package streme.sed;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.lang.reflect.InvocationTargetException;

import org.eclipse.compare.CompareConfiguration;
import org.eclipse.compare.CompareEditorInput;
import org.eclipse.compare.IStreamContentAccessor;
import org.eclipse.compare.ITypedElement;
import org.eclipse.compare.structuremergeviewer.Differencer;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.swt.graphics.Image;

public class EditorInput extends CompareEditorInput
{

	private String s1;
	private String s2;

	public EditorInput(CompareConfiguration configuration, String s1, String s2)
	{
		super(configuration);
		this.s1 = s1;
		this.s2 = s2;
	}

	protected Object prepareInput(IProgressMonitor monitor) throws InvocationTargetException, InterruptedException
	{
		Differencer d = new Differencer();
		Object diff = d.findDifferences(false, new NullProgressMonitor(), null, null, new Input(s1), new Input(s2));
		return diff;
	}

}

class Input implements ITypedElement, IStreamContentAccessor
{

	String fContent;

	public Input(String s)
	{
		fContent = s;
	}

	public String getName()
	{
		return "name";
	}

	public Image getImage()
	{
		return null;
	}

	public String getType()
	{
		return "txt";
	}

	public InputStream getContents() throws CoreException
	{
		return new ByteArrayInputStream(fContent.getBytes());
	}

}
