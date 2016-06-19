package redscript.compiler.ast

import org.objectweb.asm.commons.GeneratorAdapter

class NodeCompond(val nodes: List[NodeStatement]) extends AST
{
    override def assemble(generator: GeneratorAdapter): Unit = nodes foreach (_ assemble generator)
}
