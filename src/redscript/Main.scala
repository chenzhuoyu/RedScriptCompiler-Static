package redscript

import redscript.compiler.Parser
import redscript.compiler.ast.AST

object Main extends App
{
    def printAST(ast: Any, indent: Int, name: String): Unit = ast match
    {
        case x: AST =>
            println(("| " * indent) + s"$name: AST(${x.getClass.getSimpleName})")
            x.getClass.getDeclaredMethods foreach {
                case method =>
                    if (method.getParameterCount == 0)
                        printAST(method.invoke(x), indent + 1, method.getName)
            }

        case x: List[_] =>
            println(("| " * indent) + s"$name: List(${x.length})")
            x.zipWithIndex foreach { case (item, i) => printAST(item, indent + 1, i.toString) }

        case Some(x)  => printAST(x, indent, name)
        case Left(x)  => printAST(x, indent, name)
        case Right(x) => printAST(x, indent, name)

        case null => println(("| " * indent) + s"$name: null")
        case None => println(("| " * indent) + s"$name: None")
        case _    => println(("| " * indent) + s"$name: $ast")
    }

    val src =
        """
          | f = (x, y) => return x + y
          | println(f(1, 2))
          |
        """.stripMargin
    val parser = new Parser(src)
    printAST(parser.parse, 0, "ast")
}
