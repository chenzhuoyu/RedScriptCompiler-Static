package redscript.compiler.ast

import org.objectweb.asm.MethodVisitor

class NodeMap(val items: List[NodeMapItem]) extends AST
{
    override def assemble(method: MethodVisitor): Unit = ()
}

class NodeMapItem(val key: NodeExpr, val value: NodeExpr) extends AST
{
    override def assemble(method: MethodVisitor): Unit = ()
}
