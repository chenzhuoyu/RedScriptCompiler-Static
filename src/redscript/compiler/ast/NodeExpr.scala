package redscript.compiler.ast

import org.objectweb.asm.MethodVisitor

class NodeExpr(val left: Either[NodeExpr, NodeValue], val op: String, val right: NodeExpr) extends AST
{
    override def assemble(method: MethodVisitor): Unit = ()
}
