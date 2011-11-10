package streme.sed.editors;

import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.ITextDoubleClickStrategy;
import org.eclipse.jface.text.TextAttribute;
import org.eclipse.jface.text.presentation.IPresentationReconciler;
import org.eclipse.jface.text.presentation.PresentationReconciler;
import org.eclipse.jface.text.rules.DefaultDamagerRepairer;
import org.eclipse.jface.text.rules.ITokenScanner;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.jface.text.source.SourceViewerConfiguration;

public class StremeConfiguration extends SourceViewerConfiguration
{

  private XMLDoubleClickStrategy doubleClickStrategy;
  private ITokenScanner scanner;
  private ColorManager colorManager;

  public StremeConfiguration(ColorManager colorManager)
  {
    this.colorManager = colorManager;
  }

  public String[] getConfiguredContentTypes(ISourceViewer sourceViewer)
  {
    return new String[] { IDocument.DEFAULT_CONTENT_TYPE, StremePartitionScanner.BLOCK_COMMENT};
  }

  public ITextDoubleClickStrategy getDoubleClickStrategy(ISourceViewer sourceViewer, String contentType)
  {
    if (doubleClickStrategy == null)
      doubleClickStrategy = new XMLDoubleClickStrategy();
    return doubleClickStrategy;
  }

  protected ITokenScanner getStremeTokenScanner()
  {
    if (scanner == null)
    {
      scanner = new StremeTokenScanner(colorManager);
    }
    return scanner;
  }

  public IPresentationReconciler getPresentationReconciler(ISourceViewer sourceViewer)
  {
    PresentationReconciler reconciler = new PresentationReconciler();
    DefaultDamagerRepairer dr = new DefaultDamagerRepairer(getStremeTokenScanner());
    reconciler.setDamager(dr, IDocument.DEFAULT_CONTENT_TYPE);
    reconciler.setRepairer(dr, IDocument.DEFAULT_CONTENT_TYPE);
//    dr = new DefaultDamagerRepairer(getStremeTokenScanner());
//    reconciler.setDamager(dr, IDocument.DEFAULT_CONTENT_TYPE);
//    reconciler.setRepairer(dr, IDocument.DEFAULT_CONTENT_TYPE);
    return reconciler;
  }
}