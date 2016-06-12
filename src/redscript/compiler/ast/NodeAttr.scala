package redscript.compiler.ast
import org.objectweb.asm.MethodVisitor

class NodeAttr(val name: NodeName, var action: Int) extends AST
{
    override def assemble(method: MethodVisitor): Unit = ()
}

object NodeAttr
{
    final val Get  = 0
    final val Set  = 1
    final val Call = 2
}
