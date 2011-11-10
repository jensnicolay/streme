package streme.lang.data;


public class DataTemplate
{
  private Object template;
  
  public DataTemplate(Object template)
  {
    super();
    this.template = template;
  }
  
  public Object substitute(Object... args)
  {
    return substitute(template, args);
  }

  private Object substitute(Object source, final Object[] args)
  {
    if (source instanceof Sym)
    {
      Sym sym = (Sym) source;
      String name = sym.getName();
      if (name.startsWith("~"))
      {
        int index = Integer.parseInt(name.substring(1));
        return args[index];
      }
      return source;
    }
    if (source instanceof Pair)
    {
      return ((Pair) source).map(new Lst.Mapper()
      {
        public Object map(Object car)
        {
          return substitute(car, args);
        }
      });
    }
    return source;
  }
  
  public static void main(String[] args)
  {
    DataTemplate template = new DataTemplate(new Parser2().parse("(define ~0 'something)"));
    System.out.println(template.substitute(new Sym("heyhey")));
  }
  
}
