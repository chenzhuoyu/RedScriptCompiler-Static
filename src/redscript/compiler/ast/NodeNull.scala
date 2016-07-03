package redscript.compiler.ast

import org.objectweb.asm.Opcodes
import org.objectweb.asm.commons.GeneratorAdapter

class NodeNull extends AST
{
    vtype = null
    override def assemble(generator: GeneratorAdapter): Unit = generator.visitInsn(Opcodes.ACONST_NULL)
}
