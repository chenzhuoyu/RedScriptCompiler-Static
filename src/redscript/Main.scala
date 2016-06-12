package redscript

import redscript.compiler.Parser
import redscript.compiler.ast.AST

object Main extends App
{
    def printAST(ast: Any, indent: Int): Unit = ast match
    {
        case null =>
            println(("| " * indent) + "null")

        case x: AST =>
            println(("| " * indent) + x.getClass.getSimpleName)
            x.getClass.getDeclaredMethods foreach {
                case method =>
                    if (method.getParameterCount == 0)
                        printAST(method.invoke(x), indent + 1)
            }

        case x: List[_] =>
            println(("| " * indent) + "List")
            x foreach (printAST(_, indent + 1))

        case Left(x) =>
            println(("| " * indent) + "Left")
            printAST(x, indent + 1)

        case Right(x) =>
            println(("| " * indent) + "Right")
            printAST(x, indent + 1)

        case _ =>
            println(("| " * indent) + "Value " + ast.toString)
    }

    val src = "f[1 + 2]"
    val parser = new Parser(src)
    printAST(parser.parse, 0)
}
