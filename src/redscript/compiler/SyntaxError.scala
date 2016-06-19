package redscript.compiler

class SyntaxError(val message: String, val line: Int, val column: Int) extends Exception
{
    override def getMessage: String = s"Syntax error at line $line, column $column : $message"
}