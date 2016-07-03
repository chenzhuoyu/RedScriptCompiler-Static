package redscript.compiler.ast

import org.objectweb.asm.commons.GeneratorAdapter

class NodeImport(val names: List[NodeName]) extends AST
{
    override def assemble(generator: GeneratorAdapter): Unit = ()
}
