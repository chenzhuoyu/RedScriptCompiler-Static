package redscript.compiler.ast

import org.objectweb.asm.MethodVisitor

class NodeWhile(val expr: NodeExpr, val body: NodeStatement) extends AST
{
    override def assemble(method: MethodVisitor): Unit = ()
}
