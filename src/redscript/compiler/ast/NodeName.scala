package redscript.compiler.ast

import org.objectweb.asm.MethodVisitor

class NodeName(val name: String, val isGet: Boolean) extends AST
{
    override def assemble(method: MethodVisitor): Unit = ()
}
