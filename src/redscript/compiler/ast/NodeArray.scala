package redscript.compiler.ast

import org.objectweb.asm.MethodVisitor

class NodeArray(val items: List[NodeExpr]) extends AST
{
    override def assemble(method: MethodVisitor): Unit = ()
}
