package redscript.compiler.ast

import org.objectweb.asm.MethodVisitor

class NodeArgument(val expr: NodeExpr, val expand: Boolean) extends AST
{
    override def assemble(method: MethodVisitor): Unit = ()
}

class NodeInvokeAttr(val attr: NodeName, val invoke: NodeInvokeStack) extends AST
{
    override def assemble(method: MethodVisitor): Unit = ()
}

class NodeInvokeStack(val args: List[NodeArgument]) extends AST
{
    override def assemble(method: MethodVisitor): Unit = ()
}
