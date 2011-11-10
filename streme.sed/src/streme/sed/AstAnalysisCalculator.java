package streme.sed;

import java.util.List;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.text.IDocument;
import org.eclipse.ui.texteditor.ITextEditor;

import streme.lang.analysis.IpdAnalysis;
import streme.lang.analysis.IpdAnalyzer;
import streme.lang.analysis.ParentAnalysis;
import streme.lang.analysis.ParentAnalyzer;
import streme.lang.analysis.StremeAstAnalysis;
import streme.lang.analysis.VarPointerAnalysis;
import streme.lang.analysis.VarPointerAnalyzer;
import streme.lang.ast.Node;
import streme.lang.ast.impl.StremeSpDataCompiler2;
import streme.lang.data.Lst;
import streme.lang.data.SpParser2;

public class AstAnalysisCalculator
{

	public static StremeAstAnalysis compute(ITextEditor source)
	{
		IDocument document = source.getDocumentProvider().getDocument(source.getEditorInput());
		StremeAstAnalysis analysis = compute(document.get());
		return analysis;
	}

	private static StremeAstAnalysis compute(String source)
	{
		SpParser2 parser = new SpParser2(source);
		List<Object> datas = parser.all();
		StremeSpDataCompiler2 compiler = new StremeSpDataCompiler2(parser.getSps());
		Node ast = compiler.compileBody(Lst.valueOf(datas));
		ParentAnalysis parentAnalysis = new ParentAnalyzer().analyze(ast);
		VarPointerAnalysis varPointerAnalysis = new VarPointerAnalyzer(parentAnalysis).analyze(ast);
		IpdAnalysis ipdAnalysis = new IpdAnalyzer(12, true, varPointerAnalysis).analyze(ast);
		return new StremeAstAnalysis(ast, parentAnalysis, varPointerAnalysis, ipdAnalysis);
	}
	// private ITextEditor currentSource;
	private volatile ITextEditor requestedSource;
	private boolean calculating;
	private boolean recalc;
	private Object requestLock = new Object();

	private StremeAstAnalysis astAnalysis;

	public void calculateFor(ITextEditor source, boolean force)
	{
		synchronized (requestLock)
		{
			if (force)
			{
				recalc = true;
			}
			else if (source.equals(requestedSource))
			{
				return;
			}
			requestedSource = source;
			if (calculating)
			{
				return;
			}
			calculating = true;
			new Job("calculateFor")
			{
				protected IStatus run(IProgressMonitor monitor)
				{
					try
					{
						while (true)
						{
							ITextEditor source = requestedSource;
							StremeAstAnalysis analysis = compute(source);
							synchronized (requestLock)
							{
								if (recalc)
								{
									recalc = false;
								}
								else if (source.equals(requestedSource))
								{
									astAnalysis = analysis;
									calculating = false;
									requestLock.notifyAll();
									return Status.OK_STATUS;
								}
							}
						}
					}
					catch (Exception e)
					{
						synchronized (requestLock)
						{
							astAnalysis = null;
							calculating = false;
							requestedSource = null;
							requestLock.notifyAll();
							return new Status(IStatus.ERROR, Activator.PLUGIN_ID, "error calculating ast analysis", e);
						}
					}
				}
			}.run(null);
		}
	}

	public StremeAstAnalysis get()
	{
		synchronized (requestLock)
		{
			while (calculating)
			{
				try
				{
					requestLock.wait();
				}
				catch (InterruptedException e)
				{
					e.printStackTrace();
				}
			}
			return astAnalysis;
		}
	}

}
