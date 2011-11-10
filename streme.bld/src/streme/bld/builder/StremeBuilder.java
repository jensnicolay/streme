package streme.bld.builder;

import java.io.BufferedReader;
import java.io.CharArrayReader;
import java.io.CharArrayWriter;
import java.io.InputStreamReader;
import java.util.List;
import java.util.Map;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IResourceDeltaVisitor;
import org.eclipse.core.resources.IResourceVisitor;
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;

import streme.lang.ast.Node;
import streme.lang.ast.impl.StremeSpDataCompiler2;
import streme.lang.data.Lst;
import streme.lang.data.SpParser2;
import streme.lang.data.StremeSpException;

public class StremeBuilder extends IncrementalProjectBuilder
{

  class SampleDeltaVisitor implements IResourceDeltaVisitor
  {

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.core.resources.IResourceDeltaVisitor#visit(org.eclipse.core.resources.IResourceDelta)
     */
    public boolean visit(IResourceDelta delta) throws CoreException
    {
      IResource resource = delta.getResource();
      switch (delta.getKind())
      {
        case IResourceDelta.ADDED:
          // handle added resource
          checkXML(resource);
          break;
        case IResourceDelta.REMOVED:
          // handle removed resource
          break;
        case IResourceDelta.CHANGED:
          // handle changed resource
          checkXML(resource);
          break;
      }
      // return true to continue visiting children.
      return true;
    }
  }

  class SampleResourceVisitor implements IResourceVisitor
  {

    public boolean visit(IResource resource)
    {
      checkXML(resource);
      // return true to continue visiting children.
      return true;
    }
  }

  public static final String BUILDER_ID = "streme.bld.stremeBuilder";
  private static final String MARKER_TYPE = "streme.bld.xmlProblem";

  private void addMarker(IFile file, String message, int pos, int endPos, int severity)
  {
    try
    {
      IMarker marker = file.createMarker(MARKER_TYPE);
      marker.setAttribute(IMarker.MESSAGE, message);
      marker.setAttribute(IMarker.SEVERITY, severity);
      //marker.setAttribute(IMarker.LINE_NUMBER, lineNumber);
			marker.setAttribute(IMarker.CHAR_START, pos);
      marker.setAttribute(IMarker.CHAR_END, endPos);
    }
    catch (CoreException e)
    {
    }
  }

  private void addMarker(IFile file, String message, int lineNumber, int severity)
  {
    try
    {
      IMarker marker = file.createMarker(MARKER_TYPE);
      marker.setAttribute(IMarker.MESSAGE, message);
      marker.setAttribute(IMarker.SEVERITY, severity);
      marker.setAttribute(IMarker.LINE_NUMBER, lineNumber);
    }
    catch (CoreException e)
    {
    }
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.eclipse.core.internal.events.InternalBuilder#build(int, java.util.Map,
   * org.eclipse.core.runtime.IProgressMonitor)
   */
  protected IProject[] build(int kind, Map args, IProgressMonitor monitor) throws CoreException
  {
    if (kind == FULL_BUILD)
    {
      fullBuild(monitor);
    }
    else
    {
      IResourceDelta delta = getDelta(getProject());
      if (delta == null)
      {
        fullBuild(monitor);
      }
      else
      {
        incrementalBuild(delta, monitor);
      }
    }
    return null;
  }

  void checkXML(IResource resource)
  {
    if (resource instanceof IFile && resource.getName().endsWith(".str"))
    {
      IFile file = (IFile) resource;
      
//			IDocumentProvider documentProvider= getDocumentProvider();
//			if (documentProvider == null)
//				return Status.CANCEL_STATUS;
//
//			IAnnotationModel annotationModel= documentProvider.getAnnotationModel(getEditorInput());
//			if (annotationModel == null)
//				return Status.CANCEL_STATUS;

  
      deleteMarkers(file);
      try
      {
        BufferedReader reader = new BufferedReader(new InputStreamReader(file.getContents()));
        CharArrayWriter writer = new CharArrayWriter();
        char[] buffer = new char[1024];
        int size;
        while ((size = reader.read(buffer)) > -1)
        {
          writer.write(buffer, 0, size);
        }
        SpParser2 parser = new SpParser2(new CharArrayReader(writer.toCharArray()));
        List<Object> datas = parser.all();
        StremeSpDataCompiler2 compiler = new StremeSpDataCompiler2(parser.getSps());
        Node ast = compiler.compileBody(Lst.valueOf(datas));
      }
      catch (StremeSpException sspe)
      {
        StremeBuilder.this.addMarker(file, sspe.getMessage(), sspe.getPos(), sspe.getPos() + sspe.getLength(), IMarker.SEVERITY_ERROR);
      }
      catch (Exception e1)
      {
        StremeBuilder.this.addMarker(file, e1.getMessage(), 1, IMarker.SEVERITY_ERROR);
      }
    }
  }

  private void deleteMarkers(IFile file)
  {
    try
    {
      file.deleteMarkers(MARKER_TYPE, false, IResource.DEPTH_ZERO);
    }
    catch (CoreException ce)
    {
    }
  }

  protected void fullBuild(final IProgressMonitor monitor) throws CoreException
  {
    try
    {
      getProject().accept(new SampleResourceVisitor());
    }
    catch (CoreException e)
    {
    }
  }


  protected void incrementalBuild(IResourceDelta delta, IProgressMonitor monitor) throws CoreException
  {
    // the visitor does the work.
    delta.accept(new SampleDeltaVisitor());
  }
}
