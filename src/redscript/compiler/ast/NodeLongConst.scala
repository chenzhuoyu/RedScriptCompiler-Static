package redscript.compiler.ast

import org.objectweb.asm.MethodVisitor

class NodeLongConst(val value: BigInt) extends AST
{
    vtype = classOf[BigInt]
    override def assemble(method: MethodVisitor): Unit = method.visitLdcInsn(value)
}
