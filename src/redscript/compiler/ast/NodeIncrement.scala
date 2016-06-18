package redscript.compiler.ast

import org.objectweb.asm.MethodVisitor

class NodeIncrement(val target: NodeValue, val op: String, val expr: NodeExpr) extends AST
{
    override def assemble(method: MethodVisitor): Unit = ()
}
