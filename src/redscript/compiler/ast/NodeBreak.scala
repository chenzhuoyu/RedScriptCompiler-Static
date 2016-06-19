package redscript.compiler.ast

import org.objectweb.asm.commons.GeneratorAdapter

class NodeBreak extends AST
{
    override def assemble(generator: GeneratorAdapter): Unit = ()
}
