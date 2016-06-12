package redscript.compiler.ast

import org.objectweb.asm.{MethodVisitor, Opcodes}

class NodeIntConst(val value: Long) extends AST
{
    vtype = classOf[Long]
    override def assemble(method: MethodVisitor): Unit = value match
    {
        case 0 => method.visitInsn(Opcodes.LCONST_0)
        case 1 => method.visitInsn(Opcodes.LCONST_1)
        case _ => method.visitLdcInsn(value)
    }
}
