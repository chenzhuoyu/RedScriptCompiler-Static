package redscript.compiler.ast

import org.objectweb.asm.MethodVisitor

class NodeAssignment(val targets: List[NodeValue], val expr: NodeExpr) extends AST
{
    override def assemble(method: MethodVisitor): Unit = ()
}
