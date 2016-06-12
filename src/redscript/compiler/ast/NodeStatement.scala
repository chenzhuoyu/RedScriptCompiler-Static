package redscript.compiler.ast

import org.objectweb.asm.{MethodVisitor, Opcodes}

class NodeStatement(val node: AST) extends AST
{
    override def assemble(method: MethodVisitor): Unit =
    {
        node.assemble(method)
        node match
        {
            case _: NodeExpr  => method.visitInsn(Opcodes.POP)
            case _: NodeValue => method.visitInsn(Opcodes.POP)
            case _ =>
        }
    }
}
