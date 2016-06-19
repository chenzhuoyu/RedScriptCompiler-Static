package redscript.compiler.ast

import org.objectweb.asm.commons.GeneratorAdapter

class NodeContinue extends AST
{
    override def assemble(generator: GeneratorAdapter): Unit = ()
}
