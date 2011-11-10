package streme.lang.analysis;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import streme.lang.analysis.LexicalParentWalker.Acceptor;
import streme.lang.ast.AstAnalyzer;
import streme.lang.ast.AstVisitor;
import streme.lang.ast.Node;
import streme.lang.ast.Ref;
import streme.lang.ast.SetVar;
import streme.lang.ast.Var;
import streme.lang.ast.impl.StremeDataCompiler;
import streme.lang.data.Parser2;
import streme.lang.data.Sym;

/**
 * Resolves lexical scoping by letting Refs point to their defining/binding Var, and vice versa. Accepts all nodes and
 * kinds. Does not rely on alpha conversion.
 */
public class VarPointerAnalyzer implements AstAnalyzer<VarPointerAnalysis>
{
  public static Var lookup(final Sym name, Node current, ParentAnalysis parentAnalysis)
  {
    final Var[] varHolder = new Var[1];
    new LexicalParentWalker(parentAnalysis).visit(current, new Acceptor()
    {
      public boolean accept(Var var)
      {
        if (var.getName().equals(name))
        {
          varHolder[0] = var;
          return false;
        }
        return true;
      }
    });
    return varHolder[0];
  }

  private ParentAnalysis parentAnalysis;

  public VarPointerAnalyzer(ParentAnalysis parentAnalysis)
  {
    super();
    this.parentAnalysis = parentAnalysis;
  }

  public List<Var> inScope(Node node)
  {
    final List<Var> vars = new ArrayList<Var>();
    new LexicalParentWalker(parentAnalysis).visit(node, new Acceptor()
    {
      public boolean accept(Var var)
      {
        vars.add(var);
        return true;
      }
    });
    return vars;
  }
  
  public VarPointerAnalysis analyze(Node ast)
  {
    final Map<Ref, Var> readVars = new HashMap<Ref, Var>();
    final Map<SetVar, Var> writtenVars = new HashMap<SetVar, Var>();
    final Map<Var, Set<Ref>> readRefs = new HashMap<Var, Set<Ref>>();
    final Map<Var, Set<SetVar>> writeRefs = new HashMap<Var, Set<SetVar>>();
    ast.accept(new AstVisitor()
    {
      public void visitRef(Ref ref)
      {
        Var var = lookup(ref.getName(), ref, parentAnalysis);
        if (var != null)
        {
          readVars.put(ref, var);
          Set<Ref> refs = (Set<Ref>) readRefs.get(var);
          if (refs == null)
          {
            refs = new HashSet<Ref>();
            readRefs.put(var, refs);
          }
          refs.add(ref);
        }
      }

      public boolean visitSetVar(SetVar setVar)
      {
        Var var = lookup(setVar.getVar().getName(), setVar, parentAnalysis);
        writtenVars.put(setVar, var);
        Set<SetVar> refs = writeRefs.get(var);
        if (refs == null)
        {
          refs = new HashSet<SetVar>();
          writeRefs.put(var, refs);
        }
        refs.add(setVar);
        return true;
      }
    });
    return new VarPointerAnalysis()
    {
      public Var getVarRead(Ref ref)
      {
        return readVars.get(ref);
      }

      public Var getVarWritten(SetVar setVar)
      {
        return writtenVars.get(setVar);
      }

      public Set<Ref> getReadRefs(Var var)
      {
        Set<Ref> s = readRefs.get(var);
        return s == null ? Collections.<Ref> emptySet() : s;
      }

      public Set<SetVar> getWriteRefs(Var var)
      {
        Set<SetVar> s = writeRefs.get(var);
        return s == null ? Collections.<SetVar> emptySet() : s;
      }
      
      public List<Var> inScope(Node node)
      {
        return VarPointerAnalyzer.this.inScope(node);
      }
    };
  }

  public static void main(String[] args)
  {
    Parser2 parser = new Parser2();
    StremeDataCompiler compiler = new StremeDataCompiler();
    String source = "(letrec ((fib (lambda (n) (if (< n 2) n (let ((a (fib (- n 2))) (b (fib (- n 1)))) (+ a b)))))) (fib 20))";
    Node ast = compiler.compile(parser.parse(source));
    ParentAnalyzer parentAnalyzer = new ParentAnalyzer();
    ParentAnalysis parentAnalysis = parentAnalyzer.analyze(ast);
    VarPointerAnalyzer varPointerAnalyzer = new VarPointerAnalyzer(parentAnalysis);
    VarPointerAnalysis varPointerAnalysis = varPointerAnalyzer.analyze(ast);
  }
}
