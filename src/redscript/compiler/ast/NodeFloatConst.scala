package redscript.compiler.ast

import org.objectweb.asm.{MethodVisitor, Opcodes}

class NodeFloatConst(val value: Double) extends AST
{
    vtype = classOf[Double]
    override def assemble(method: MethodVisitor): Unit = value match
    {
        case 0.0 => method.visitInsn(Opcodes.DCONST_0)
        case 1.0 => method.visitInsn(Opcodes.DCONST_1)
        case _   => method.visitLdcInsn(value)
    }
}
