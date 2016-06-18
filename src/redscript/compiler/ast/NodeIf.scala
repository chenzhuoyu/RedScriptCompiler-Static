package redscript.compiler.ast

import org.objectweb.asm.MethodVisitor

class NodeIf(val expr: NodeExpr, val success: NodeStatement, val failed: Option[NodeStatement]) extends AST
{
    override def assemble(method: MethodVisitor): Unit = ()
}
