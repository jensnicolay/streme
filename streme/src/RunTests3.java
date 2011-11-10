import java.io.BufferedReader;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.io.StringReader;
import java.net.URL;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Map;
import java.util.Properties;

import streme.lang.StremeException;
import streme.lang.ast.AstDataCompiler;
import streme.lang.ast.Node;
import streme.lang.ast.analysis.AnfConverter;
import streme.lang.ast.analysis.LetBodyApplicationRemover;
import streme.lang.ast.analysis.RenamingStrategy;
import streme.lang.ast.analysis.Undefiner;
import streme.lang.ast.analysis.ipda.BruteForceParallelizer2;
import streme.lang.ast.analysis.ipda.Ipda;
import streme.lang.ast.analysis.ipda.IpdaDependencyCounter;
import streme.lang.ast.analysis.ipda.LetBodyNodeAdder;
import streme.lang.ast.impl.AlphaConverter;
import streme.lang.ast.impl.LexicalSimplifiers;
import streme.lang.ast.impl.NodeCounter;
import streme.lang.ast.impl.ParallelConstructsRewriter;
import streme.lang.ast.impl.Printer;
import streme.lang.ast.impl.StremeDataCompiler;
import streme.lang.ast.impl.SyntaxSimplifier;
import streme.lang.data.Parser2;
import streme.lang.eval.MacroExpander;

public class RunTests3
{
  private PrintWriter out;
  private int numberOfCores;
  private String globalName;
  private Parser2 parser = new Parser2();
  private AnfConverter anfConverter = new AnfConverter(RenamingStrategy.NUMBER_RENAMING_STRATEGY);
  private SyntaxSimplifier simp = new SyntaxSimplifier();
  private Properties properties;

  public RunTests3(String globalName, int numberOfCores, String environment) throws IOException
  {
    super();
    this.numberOfCores = numberOfCores;
    this.globalName = globalName;
    properties = new Properties();
    properties.load(new StringReader(load(environment + ".properties")));
    System.out.println("loaded properties for environment '" + environment + "'");
    String fileName = this.globalName + "-" + environment + ".txt";
    final File file = new File(fileName);
    out = new PrintWriter(file);
    System.out.println("writing to file " + file.getAbsolutePath());
    long globalStart = System.currentTimeMillis();
    try
    {
//      runTest("fib", load("gabriel/fib.str"), 1, false);
//      runTest("tak", load("gabriel/tak.str"), 1, false);
      runTest("nboyer", load("gabriel/nboyer.str"), 1, false);
//      runTest("nboyer-true", load("gabriel/nboyer.str"), 1, true);
//      runTest("mergesort", load("other/mergesort.str"), 1, false);
//      runTest("mergesort-true", load("other/mergesort.str"), 1, true);
//      runTest("quicksort", load("other/quicksort.str"), 1, false);
//      runTest("quicksort-true", load("other/quicksort.str"), 1, true);
      runTest("nqueens", load("other/nqueens.str"), 1, false);
//      runTest("nqueens-true", load("other/nqueens.str"), 1, true);

//      runTest("boyer", load("gabriel/boyer.str"), 1, false);
//      runTest("boyer-true", load("gabriel/boyer.str"), 1, true);
//      runTest("triangl", load("gabriel/triangl.str"), 1, false);
//      runTest("triangl-true", load("gabriel/triangl.str"), 1, true);
//      runTest("browse", load("gabriel/browse.str"), 25, false);
//      runTest("browse-true", load("gabriel/browse.str"), 25, true);
//      runTest("graphs", load("other/graphs.str"), 25, false);
//      runTest("graphs-true", load("other/graphs.str"), 25, true);
////      runTest("factor", load("other/factor.str"), 10, false);
////      runTest("factor-true", load("other/factor.str"), 10, true);
      // runManual("pquicksort-anf", load("manual/pquicksort-anf.str"), 10, true); ANF hangs!!!
      // runManual("pmergesort", load("manual/pmergesort.str"), 10, false);
      // runManual("pquicksort", load("manual/pquicksort.str"), 10, false);
      // runTest("diviter", load("gabriel/diviter.str"), 1000); too short
      // runTest("divrec", load("gabriel/divrec.str"), 100); too short
      // runTest("mazefun", load("other/mazefun.str"), 20); NPE!
      // runTest("cpstak", load("gabriel/cpstak.str"), 4); too short
      // runTest("pnpoly", load("gabriel/pnpoly.str"), 1000); too short
//      // runTest("mbrotcore", load("other/mbrotcore.str"), 1000);
    }
    catch (Throwable t)
    {
      t.printStackTrace(out);
      t.printStackTrace();
    }
    long globalEnd = System.currentTimeMillis();
    out.flush();
    out.close();
    System.out.println("wrote to file " + file.getAbsolutePath());
    System.out.println("global time: " + ((globalEnd - globalStart) / 1000 / 60) + " minutes");
    System.out.println("Thank you, please come again!");
  }

  public void runTest(String sourceName, String source, final int numIter, boolean parallelizeApplicationsOnly)
  {
    System.gc();
    String info = "\n********\n" + sourceName + " (x" + numIter + ") " + (parallelizeApplicationsOnly ? "(par only apps)" : "(par everything)") + "\n";
    out.println(info);
    System.out.println(info);
    for (Map.Entry<Object, Object> entry : properties.entrySet())
    {
      source = source.replace((String) entry.getKey(), (String) entry.getValue());
    }
    Node alphaAst = getAlphaAst(parser, source);
    Node anf = getAnf(alphaAst);
    Node sanf = simplify(anf);
    
    Node pre = null;
    Node post = anf;
 //   while (!post.nodeEquals(pre))
    {
      pre = post;
      
      Ipda ipda = new Ipda(4);
      System.out.println("running ipda");
      long ipdaStart = System.currentTimeMillis();
      ipda.analyze(post);
      long ipdaEnd = System.currentTimeMillis();
      out.println("ipda time: " + (ipdaEnd - ipdaStart));
      IpdaDependencyCounter ipdac = new IpdaDependencyCounter(ipda);
      post.accept(ipdac);
      System.out.println("ipda dependencies: " + ipdac.getCounter());
      out.println("ipda dependencies: " + ipdac.getCounter());
      System.out.println("running bfp");
      BruteForceParallelizer2 bfp = new BruteForceParallelizer2(ipda);
      bfp.setOnlyApplications(parallelizeApplicationsOnly);
      long bfpStart = System.currentTimeMillis();
      Node bfpAnf = bfp.rewrite(post);
      post = simplify(bfpAnf); // Node panf = ...
      long bfpEnd = System.currentTimeMillis();
      out.println("bfp time:  " + (bfpEnd - bfpStart));
    }
    Node panf = post;
    
    System.gc();
    Node corePast = new ParallelConstructsRewriter().rewrite(panf);
    NodeCounter nc = new NodeCounter();
    Object touchPattern = parser.parse("(touch ?)");
    nc.countPattern(touchPattern);
    corePast.accept(nc);
    out.println("node count: total=" + nc.getTotalCount() + " futures=" + nc.getTypeCount(Node.Type.FUTURE)
        + " touches=" + nc.getPatternCount(touchPattern));
    //ParentAnnotator.annotate(ast);
    final Node corePanf = SyntaxSimplifier.simplify(anfConverter.rewrite(corePast));
    System.out.println(Printer.print(corePanf));
    
    
//    Logging.setup(Level.OFF);
//    System.out.println("running anf");
//    TanfEvaluator anfEvaluator = new TanfEvaluator(null);
//    TanfStreme anfStreme = new TanfStreme(null);
//    System.gc();
//    Object[] anfRet = new Object[numIter];
//    long anfStart = System.currentTimeMillis();
//    for (int i = 0; i < numIter; i++)
//    {
//      anfRet[i] = AstEvaluators.evaluate(anfEvaluator, sanf, anfStreme.globalEnv());
//    }
//    long anfEnd = System.currentTimeMillis();
//    System.out.println(Arrays.toString(anfRet));
//    long anfTime = anfEnd - anfStart;
//    System.out.println(anfTime);
//    out.println("sanf time:  " + anfTime);
//    final long[] panfTimes = new long[numberOfCores];
//    for (int t = 0; t < numberOfCores; t++) 
//    {
//      int parallelism = t + 1;
//      int maxFutures = t * 2;
//      
//      final int c = t;
//      ExecutorService panfExecutor = new ForkJoinPool(parallelism, ForkJoinPool.defaultForkJoinWorkerThreadFactory, null, false);
//      final TanfEvaluator panfEvaluator = new TanfEvaluator(panfExecutor, maxFutures);
//      final TanfStreme panfStreme = new TanfStreme(panfExecutor);
//      System.out.println("panf Streme parallelism " + parallelism + ", max futures " + maxFutures + " (" + sourceName
//          + ")");
//      out.println("*** panf Streme parallelism " + parallelism + ", max futures " + maxFutures);
//      final Object[] panfRet = new Object[numIter];
//      try
//      {
//        panfExecutor.submit(new Callable<Void>()
//        {
//          public Void call() throws Exception
//          {
//            long panfStart = System.currentTimeMillis();
//            for (int i = 0; i < numIter; i++)
//            {
//              panfRet[i] = AstEvaluators.evaluate(panfEvaluator, corePanf, panfStreme.globalEnv());
//            }
//            long panfEnd = System.currentTimeMillis();
//            System.out.println(Arrays.toString(panfRet));
//            long panfTime = panfEnd - panfStart;
//            System.out.println(panfTime);
//            out.print("panf time: " + panfTime);
//            panfTimes[c] = panfTime;
//            double p = (((panfTime - panfTimes[0]) * 10000d) / ((double) panfTimes[0]));
//            out.println(" " + Math.round(p) / 100d + "%");
//            return null;
//          }
//        }).get();
//        panfExecutor.shutdown();
//      }
//      catch (Exception e)
//      {
//        e.printStackTrace(out);
//        e.printStackTrace();
//      }
//    }
//    out.println("chart url: " + dumpTimeChart(globalName + "-" + sourceName, panfTimes, "0000FF"));
    out.println("\n********");
    out.flush();
  }
  
  public Node simplify(Node ast)
  {
    Node pre = null;
    Node post = ast;
    while (!post.nodeEquals(pre))
    {
      pre = post;
      post = SyntaxSimplifier.simplify(pre);
      post = LexicalSimplifiers.simplify(post);
    }
    return post;
  }

  public Node getAnf(Node alphaAst)
  {
    Node anf = anfConverter.rewrite(alphaAst);
    LetBodyApplicationRemover lbar = new LetBodyApplicationRemover(RenamingStrategy.NUMBER_RENAMING_STRATEGY);
    LetBodyNodeAdder lbna = new LetBodyNodeAdder(RenamingStrategy.NUMBER_RENAMING_STRATEGY);
    Node lanf = lbar.rewrite(anf);
    return lanf;
  }

  public Node getAlphaAst(Parser2 parser, String source)
  {
    Object data = parser.parse(source);
    MacroExpander expander = new MacroExpander();
    expander.setLetToLambda(false);
    expander.setLetrecToLambda(false);
    Object expanded = expander.rewrite(data);
    AstDataCompiler dataCompiler = new StremeDataCompiler();
    Node ast = Undefiner.undefine(dataCompiler.compile(expanded));
    AlphaConverter alphaConverter = new AlphaConverter(RenamingStrategy.NUMBER_RENAMING_STRATEGY);
    Node alphaAst = alphaConverter.rewrite(ast);
    return alphaAst;
  }

  private String load(String name)
  {
    String fileName = "test/" + name;
    try
    {
      InputStream resourceAsStream = getClass().getClassLoader().getResourceAsStream(fileName);
      BufferedReader reader = new BufferedReader(new InputStreamReader(resourceAsStream));
      StringBuilder b = new StringBuilder();
      String s;
      while ((s = reader.readLine()) != null)
      {
        b.append(s).append("\n");
      }
      reader.close();
      return b.toString();
    }
    catch (Exception e)
    {
      throw new StremeException("cannot load " + fileName, e);
    }
  }

  private String dumpTimeChart(String name, long[] data, String color)
  {
    // http://chart.apis.google.com/chart?cht=lc&chd=t:200,140,120,110,105,102,101,100&chds=90,210&chs=640x400&chxt=x,x,y,y&chxr=0,1,8,1|2,90,210&chxl=1:|%23threads|3:|time%20(s)&chxp=1,50|3,50&chg=14.29,16.66&chls=5&chco=0000FF66&chm=d,0000FF,0,-1,15&chma=0,10,20,0
    long max = 0;
    long min = Long.MAX_VALUE;
    StringBuilder urlDataSb = new StringBuilder();
    int n = data.length;
    for (long d : data)
    {
      max = Math.max(max, d);
      min = Math.min(min, d);
      urlDataSb.append(d).append(",");
    }
    String urlData = urlDataSb.substring(0, urlDataSb.length() - 1);
    double low = min - min * 0.1d;
    double high = max + max * 0.05d;
    double urlXGrid = 100.0d / (n - 1);
    double urlYGrid = 10d; // :(
    String urlStr = "http://chart.apis.google.com/chart?cht=lc&chd=t:" + urlData + "&chds=" + low + "," + high
        + "&chs=640x400&chxt=x,x,y,y&chxr=0,1," + n + ",1|2," + low + "," + high
        + "&chxl=1:|%23threads|3:|time%20(ms)&chxp=1,50|3,50&chg=" + urlXGrid + "," + urlYGrid + "&chls=5&chco="
        + color + "66&chm=d," + color + ",0,-1,15&chma=0,10,20,0";
    writeChart(name, urlStr);
    return urlStr;
  }

  private String dumpSpeedupChart(String name, long[] data, String color)
  {
    // http://chart.apis.google.com/chart?cht=lc&chd=t:200,140,120,110,105,102,101,100&chds=90,210&chs=640x400&chxt=x,x,y,y&chxr=0,1,8,1|2,90,210&chxl=1:|%23threads|3:|time%20(s)&chxp=1,50|3,50&chg=14.29,16.66&chls=5&chco=0000FF66&chm=d,0000FF,0,-1,15&chma=0,10,20,0
    int n = data.length;
    long first = data[0];
    int max = 100;
    int min = 100;
    StringBuilder urlDataSb = new StringBuilder().append(100).append(",");
    for (int i = 1; i < n; i++)
    {
      long d = data[i];
      int m = Math.round(100 * first / d);
      max = Math.max(max, m);
      min = Math.min(min, m);
      urlDataSb.append(m).append(",");
    }
    String urlData = urlDataSb.substring(0, urlDataSb.length() - 1);
    System.out.println("urlData " + urlData);
    System.out.println("min " + min + " max " + max);
    int low = Math.round(min / 10f) * 10 - 10;
    int high = Math.round(max / 10f) * 10 + 10;
    System.out.println("low " + low + " high " + high);
    double urlXGrid = 100d / (n - 1);
    double urlYGrid = 2000d / (high - low); // 1000
    System.out.println("urlXGrid " + urlXGrid + " urlYGrid " + urlYGrid);
    //|0.9x|1x|1.1x|1.2x|1.3x|1.4x|1.5x|1.6x      
    StringBuilder urlYLabels = new StringBuilder();
    for (int i = low; i <= high ; i += 20) // 10
    {
      urlYLabels.append("|").append(i / 50d).append("x"); // 100
    }
    System.out.println(urlYLabels);
    String urlStr = "http://chart.apis.google.com/chart?cht=lc&chd=t:" + urlData + "&chds=" + low + "," + high
        + "&chs=640x400&chxt=x,x,y,y&chxr=0,1," + n + ",1|2," + low + "," + high
        + "&chxl=1:|parallelism|2:" + urlYLabels + "|3:|speedup&chxp=1,50|3,50&chg=" + urlXGrid + "," + urlYGrid + "&chls=5&chco="
        + color + "66&chm=d," + color + ",0,-1,15&chma=0,10,20,0";
    writeChart(name, urlStr);
    return urlStr;
  }

  public void writeChart(String name, String urlStr)
  {
    try
    {
      URL url = new URL(urlStr);
      InputStream in = url.openStream();
      int c;
      FileOutputStream fout = new FileOutputStream(name + ".png");
      while ((c = in.read()) != -1)
      {
        fout.write(c);
      }
      fout.close();
      in.close();
    }
    catch (Exception e)
    {
      e.printStackTrace();
    }
  }

  public static void main(String[] args) throws IOException
  {
    System.out.println("Streme batch tester");
    int numberOfCores = Runtime.getRuntime().availableProcessors();
    String timestamp = new SimpleDateFormat("yyyyMMdd-HHmmss").format(new Date());
    String environment = System.getProperty("environment");
    if (environment == null)
    {
      environment = "default";
    }
    new RunTests3(timestamp, numberOfCores, environment);
  }
}
