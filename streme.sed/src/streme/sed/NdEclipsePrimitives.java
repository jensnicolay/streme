package streme.sed;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Callable;

import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.dom.AST;
import org.eclipse.jdt.core.dom.ASTParser;
import org.eclipse.jdt.core.dom.ASTRequestor;
import org.eclipse.jdt.core.dom.CompilationUnit;

import streme.lang.TCont;
import streme.lang.data.Pair;
import streme.lang.data.Sym;
import streme.lang.eval.MapEnv;
import streme.lang.eval.nd.Procedure;
import streme.lang.eval.nd.TSuccess;

public class NdEclipsePrimitives
{
  public static void loadPrimitives(MapEnv env)
  {
    env.add(new Sym("jdt-parse"), new Procedure() {@Override
    public Callable<Callable> apply1(Object icu, MapEnv env, TSuccess cont, TCont fail)
    {
    	final List l = new ArrayList();
    	ASTParser parser = ASTParser.newParser(AST.JLS3);
    	ASTRequestor requestor = new ASTRequestor()
			{
    		@Override
    		public void acceptAST(ICompilationUnit source, CompilationUnit ast)
    		{
    			l.add(Pair.cons(source, ast));
    		}
			};
			parser.createASTs(new ICompilationUnit[] {(ICompilationUnit) icu}, new String[0], requestor, null);
    	return cont.call((CompilationUnit) l.get(0), fail);
    }});
  }
}
