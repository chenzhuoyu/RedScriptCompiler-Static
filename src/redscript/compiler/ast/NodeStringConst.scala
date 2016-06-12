package redscript.compiler.ast

import org.objectweb.asm.MethodVisitor

class NodeStringConst(val value: String) extends AST
{
    vtype = classOf[String]
    override def assemble(method: MethodVisitor): Unit = method.visitLdcInsn(value)
}
