package redscript.compiler.ast

import org.objectweb.asm.MethodVisitor

class NodeIndex(val index: NodeExpr, val isGet: Boolean) extends AST
{
    override def assemble(method: MethodVisitor): Unit = ()
}
