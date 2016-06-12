package redscript.compiler.ast
import org.objectweb.asm.MethodVisitor

class NodeCompond(val nodes: List[NodeStatement]) extends AST
{
    override def assemble(method: MethodVisitor): Unit = nodes foreach (_ assemble method)
}
