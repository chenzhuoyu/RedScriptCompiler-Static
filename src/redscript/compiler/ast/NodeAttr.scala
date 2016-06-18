package redscript.compiler.ast

import org.objectweb.asm.MethodVisitor

class NodeAttr(val name: NodeName, val isGet: Boolean) extends AST
{
    override def assemble(method: MethodVisitor): Unit = ()
}
